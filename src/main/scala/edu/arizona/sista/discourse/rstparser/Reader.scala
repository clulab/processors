package edu.arizona.sista.discourse.rstparser

import java.io.File
import scala.io.Source
import java.util.regex.{Pattern,Matcher}
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import org.slf4j.LoggerFactory
import edu.arizona.sista.processors.{Document, Processor}
import edu.arizona.sista.struct.MutableNumber
import RelationDirection._
import TreeKind._
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import Reader._
import scala.collection.mutable

/**
 * Parses the output of the discourse parser (the .dis files)
 * User: mihais
 * Date: 8/6/13
 */
class Reader {
  var tokenizationMistakes = 0
  var totalTokens = 0

  /**
   * Reads a discourse tree from a .dis file
   * @param file The .dis file
   * @param proc Use this processor to parse the text
   * @param verbose If true, prints more debug messages
   * @return
   */
  def read(file:File,
           proc:Processor = null,
           simplifyRelationLabels:Boolean = true,
           verbose:Boolean = false): (DiscourseTree, Document) = {
    // read the file into a string
    val sb = new StringBuilder
    val source = Source.fromFile(file)
    for(l <- source.getLines()) {
      sb.append(cleanLine(l))
      sb.append(" ")
    }
    source.close()

    // tokenize the file
    val tokens = tokenizeDis(sb.toString())
    if(verbose) {
      var i = 0
      while(i < tokens.length) {
        println((i + 1) + ": " + tokens(i))
        i += 1
      }
    }

    // and now parse the sequence of tokens into a tree
    val offset: MutableNumber[Int] = new MutableNumber[Int](0)
    var root = parseDisTokens(tokens, offset)
    if(verbose) {
      println("Raw tree:\n" + root.toString(printChildren = true, printKind = true))
    }

    // move labels from the children nodes to parents, where they should be
    propagateLabels(root)
    if(verbose) {
      println("After label propagation:\n" + root)
    }

    // simplify labels to the set of 18 coarse relations
    if(simplifyRelationLabels) {
      simplifyLabels(root)
      if(verbose) {
        println("After label simplification:\n" + root)
      }
    }

    // make sure every node has exactly two children
    root = binarizeTree(root)
    checkBinary(root)
    if(verbose) {
      println("After binarization:\n" + root)
    }

    // count character offsets for the raw text
    val charOffset = new MutableNumber[Int](0)
    countCharOffsets(root, charOffset)

    // build the overall text, concatenating all EDUs in this tree
    val textBuilder = new ArrayBuffer[StringBuilder]()
    textBuilder += new StringBuilder
    concatenateEDUTexts(root, textBuilder)
    val text = constructText(textBuilder)
    if(verbose) {
      println("TEXT:")
      for(i <- 0 until text.size) {
        print(s"Sentence #$i: " + text(i))
      }
    }

    // parse the whole text
    val doc = annotate(text, proc)
    for(s <- doc.sentences) totalTokens += s.size
    if(verbose) {
      println("DOC:")
      printDocTokens(doc)
      println()
    }

    // align annotations with EDUs
    if(doc != null) {
      val senOffset = new MutableNumber[Int](0)
      val tokOffset = new MutableNumber[Int](0)

      // this creates firstToken and lastToken for *terminal* nodes
      alignAnnotations(root, doc, senOffset, tokOffset, verbose)
      // now assign firstToken and lastToken for *nonterminals*
      propagateTokenPositions(root)
    }

    //checkTerminals(root, doc)

    (root, doc)
  }

  def constructText(textBuilder:ArrayBuffer[StringBuilder]):Array[String] = {
    // remove empty lines
    val text = textBuilder.map(_.toString()).filter(_.length > 0).toArray

    // fix erroneus EOS: merge sentences when they are detected to be part of a single sentence
    val textRepairedEOS = new ArrayBuffer[String]()
    var i = 0
    while(i < text.size) {
      val b = new mutable.StringBuilder()
      val t = text(i)
      b.append(t)
      i += 1
      // if the next sentence starts with a lower case character, merge!
      while (i < text.size && text(i).charAt(0).isLower) {
        b.append(" ")
        b.append(text(i))
        //println(s"MERGED [${t}] with [${text(i)}}]")
        i += 1
      }
      textRepairedEOS.append(b.toString())
    }
    textRepairedEOS.toArray
  }

  def checkTerminals(t:DiscourseTree, doc:Document) {
    if(t.isTerminal) {
      if(t.firstToken.token == 1) {
        //println("FOUND WEIRD TOKEN AT 1")
        val sen = doc.sentences(t.firstToken.sentence)
        for(i <- 0 until math.min(4, sen.size))
          print(" " + sen.words(i))
        println()
      }
      if(t.lastToken.token == doc.sentences(t.lastToken.sentence).size - 2) {
        //println("FOUND WEIRD TOKEN AT LEN - 2")
        val sen = doc.sentences(t.lastToken.sentence)
        for(i <- math.max(0, sen.size - 4) until sen.size)
          print(" " + sen.words(i))
        println()
      }
    } else {
      for(c <- t.children)
        checkTerminals(c, doc)
    }
  }

  def annotate(text:Array[String], proc:Processor):Document = {
    if(proc == null) return null

    val doc = proc.mkDocumentFromSentences(text, keepText = false)
    proc.annotate(doc)

    doc
  }

  private def binarizeTree(t:DiscourseTree):DiscourseTree = {
    // nothing to do for terminals
    if(t.isTerminal) return t

    // not much to do for non-terminals with exactly 2 children
    if(t.children.size > 2) {
      val oldChildren = t.children
      t.children = new Array[DiscourseTree](2)
      t.children(0) = oldChildren(0)
      t.children(1) = makeBinaryChild(t, oldChildren, 1)
    }

    for(i <- 0 until t.children.size) {
      t.children(i) = binarizeTree(t.children(i))
    }

    t
  }

  private def makeBinaryChild(parent:DiscourseTree, children:Array[DiscourseTree], childrenOffset:Int):DiscourseTree = {
    if(childrenOffset == children.size - 1)
      return children(childrenOffset)

    val t = new DiscourseTree(
      parent.relationLabel,
      parent.relationDirection,
      new Array[DiscourseTree](2),
      children(childrenOffset).kind,
      null,
      (-1, -1))

    t.children(0) = children(childrenOffset)
    t.children(1) = makeBinaryChild(parent, children, childrenOffset + 1)
    t
  }

  private def checkBinary(t:DiscourseTree) {
    if(! t.isTerminal) {
      if(t.children.size != 2) {
        throw new RuntimeException("ERROR: found non-binary tree:\n" + t)
      }
      for(c <- t.children) {
        checkBinary(c)
      }
    }
  }

  private def simplifyLabels(t:DiscourseTree) {
    if(! t.isTerminal) {
      var found = false
      for(pattern <- LABEL_PATTERNS if ! found) {
        val m = pattern._1.matcher(t.relationLabel)
        if(m.find() && m.start() == 0){
          found = true
          t.relationLabel = pattern._2
        }
      }
      if(! found)
        throw new RuntimeException("ERROR: could not simplify label " + t.relationLabel)

      for(c <- t.children)
        simplifyLabels(c)
    }
  }

  private def cleanLine(l:String):String = {
    val cl = removePattern(l, TT_ERR)
    cl
  }

  private def printDocTokens(doc:Document) {
    for(s <- doc.sentences) {
      for(i <- 0 until s.size) {
        print(s.words(i) + "[" + s.startOffsets(i) + ", " + s.endOffsets(i) + "] ")
      }
      println()
    }
  }

  private def alignAnnotations(t:DiscourseTree,
                               d:Document,
                               senOffset:MutableNumber[Int],
                               tokOffset:MutableNumber[Int],
                               verbose:Boolean) {
    if(t.isTerminal) {
      // ideally, we should start a token at this offset
      // but we might be inside of one, due to different tokenization
      if(! (d.sentences(senOffset.value).startOffsets(tokOffset.value) <= t.charOffsets._1 &&
            d.sentences(senOffset.value).endOffsets(tokOffset.value) > t.charOffsets._1))
        throw new RuntimeException("ERROR: tree cannot be aligned with annotation: " +
          "could not find start character offset " + t.charOffsets._1)

      if(d.sentences(senOffset.value).startOffsets(tokOffset.value) < t.charOffsets._1 &&
         d.sentences(senOffset.value).endOffsets(tokOffset.value) > t.charOffsets._1) {
        tokenizationMistakes += 1
      }

      if(verbose)
        println("Aligning EDU: " + t.rawText + " [" + t.charOffsets._1 + ", " + t.charOffsets._2 + "]")
      t.firstToken = new TokenOffset(senOffset.value, tokOffset.value)
      if(verbose)
        println("\tFound FIRST token in sentence " + senOffset.value + " at token " + tokOffset.value)
      if(! advanceToCharacterOffset(d, t.charOffsets._2, senOffset, tokOffset))
        throw new RuntimeException("ERROR: tree cannot be aligned with annotation: " +
          "could not find end character offset " + t.charOffsets._2)
      t.lastToken = new TokenOffset(senOffset.value, tokOffset.value)
      if(verbose)
        println("\tFound LAST token in sentence " + senOffset.value + " at token " + tokOffset.value)

      // move to the next token, if necessary
      if(d.sentences(senOffset.value).endOffsets(tokOffset.value) == t.charOffsets._2) {
        if(tokOffset.value < d.sentences(senOffset.value).size - 1) {
          tokOffset.value += 1
        } else if(senOffset.value < d.sentences.size - 1) {
          senOffset.value += 1
          tokOffset.value = 0
        }
      }

    } else {
      for(c <- t.children) {
        alignAnnotations(c, d, senOffset, tokOffset, verbose)
      }
    }
  }

  private def propagateTokenPositions(t:DiscourseTree) {
    if(! t.isTerminal) {
      for(c <- t.children) {
        propagateTokenPositions(c)
      }

      t.firstToken = t.children.head.firstToken
      t.lastToken = t.children.last.lastToken
    }
  }

  private def advanceToCharacterOffset(d:Document,
                                       charOffset:Int,
                                       senOffset:MutableNumber[Int],
                                       tokOffset:MutableNumber[Int]):Boolean = {
    while(senOffset.value < d.sentences.size) {
      while(tokOffset.value < d.sentences(senOffset.value).size) {
        val end = d.sentences(senOffset.value).endOffsets(tokOffset.value)
        val start = d.sentences(senOffset.value).startOffsets(tokOffset.value)
        // ideally, we should look for end == charOffset
        // but our tokenization may be different than RST's, so we have to allow for some slack
        if(start < charOffset && end >= charOffset) {
          return true
        } else if(end > charOffset) {
          return false
        } else {
          tokOffset.value += 1
        }
      }
      senOffset.value += 1
      tokOffset.value = 0
    }
    false
  }

  private def concatenateEDUTexts(t:DiscourseTree, text:ArrayBuffer[StringBuilder]) {
    if(t.isTerminal) {
      if(t.rawText != null && t.rawText.length > 0) {
        val sent = text.last
        sent.append(t.rawText)
        // add 1 space or 1 NL after each EDU
        if(! endOfSentence(t.rawText)) {
          sent.append(" ")
        } else {
          text += new StringBuilder
        }
      }
    } else {
      for(c <- t.children)
        concatenateEDUTexts(c, text)
    }
  }

  def endOfSentence(s:String):Boolean = {
    val m = EOS.matcher(s)
    while(m.find()) {
      if(m.end() == s.length) {
        // println(s"FOUND EOS FOR EDU: [$s]")
        return true
      }
    }

    // header text, ending with --, seems to be treated as EOS in this corpus
    if(s.length > 3 && s.trim.endsWith("--") &&
      (Character.isUpperCase(s.charAt(0)) || s.charAt(0) =='(')) {
      // println(s"FOUND HEADER EOS FOR EDU: [$s]")
      return true
    }

    false
  }

  private def countCharOffsets(node:DiscourseTree, charOffset:MutableNumber[Int]) {
    if(node.isTerminal) {
      val end = charOffset.value + node.rawText.length
      node.charOffsets = new Tuple2(charOffset.value, end)
      // we will construct the overall text with 1 space character between EDUs
      charOffset.value = end + 1
    } else {
      for(c <- node.children)
        countCharOffsets(c, charOffset)
    }
  }

  private def propagateLabels(node:DiscourseTree) {
    if(node.isTerminal) {
      node.relationLabel = ""
    } else {
      var dir:RelationDirection = None
      var label = ""
      if(node.children(0).kind == Nucleus && node.children.last.kind == Satellite) {
        dir = LeftToRight
        label = node.children.last.relationLabel
      } else if(node.children.last.kind == Nucleus && node.children(0).kind == Satellite) {
        dir = RightToLeft
        label = node.children(0).relationLabel
      } else {
        assert(node.children(0).kind == node.children.last.kind)
        label = node.children(0).relationLabel
        dir = None
      }
      node.relationLabel = label.toLowerCase
      node.relationDirection = dir
      for(c <- node.children)
        propagateLabels(c)
    }
  }

  private def parseDisTokens(tokens:Array[Token], offset:MutableNumber[Int]):DiscourseTree = {
    consume("LP", tokens, offset)
    var t = consume("KIND", tokens, offset)
    val kind = t.value
    t = consume("SPAN|LEAF", tokens, offset)
    val isLeaf = t.kind == "LEAF"
    var label = ""
    if(kind != "Root") {
      t = consume("LABEL", tokens, offset)
      label = t.value
    }
    if(isLeaf) {
      t = consume("TEXT", tokens, offset)
      val text = cleanText(t.value)
      consume("RP", tokens, offset)
      new DiscourseTree(
        label,
        None,
        null,
        toTreeKind(kind),
        text,
        new Tuple2(-1, -1))
    } else {
      var endOfNode = false
      val children = new ArrayBuffer[DiscourseTree]()
      while(! endOfNode) {
        t = lookAhead(tokens, offset)
        // println(s"LOOKAHEAD: $t")
        if(t.kind == "RP") {
          endOfNode = true
        } else {
          val n = parseDisTokens(tokens, offset)
          // println("PARSED: " + n)
          children += n
        }
      }
      consume("RP", tokens, offset)
      val node = new DiscourseTree(
        label,
        None,
        children.toArray,
        toTreeKind(kind),
        null,
        null)
      node
    }
  }

  private def cleanText(t:String):String = {
    val ct = removePattern(t, Reader.P)
    ct.trim
  }

  private def removePattern(s:String, p:Pattern):String = {
    val sb = new StringBuilder
    val m = p.matcher(s)
    var offset = 0
    while(m.find(offset)) {
      sb.append(s.substring(offset, m.start()))
      offset = m.end()
    }
    if(offset < s.length) {
      sb.append(s.substring(offset))
    }
    sb.toString()
  }

  private def toTreeKind(s:String):TreeKind = {
    if(s == "Nucleus") Nucleus
    else if(s == "Satellite") Satellite
    else if(s == "Root") Root
    else throw new RuntimeException("ERROR: unknown TreeKind: " + s)
  }

  private def consume(kind:String, tokens:Array[Token], offset:MutableNumber[Int]):Token = {
    if(offset.value >= tokens.length)
      throw new RuntimeException("ERROR: end of stream reached to soon!")

    if(Pattern.compile(kind, Pattern.CASE_INSENSITIVE).matcher(tokens(offset.value).kind).matches()) {
      val t = tokens(offset.value)
      offset.value += 1
      t
    } else {
      val v = tokens(offset.value).kind
      throw new RuntimeException(s"ERROR: expected $kind but seen $v at position $offset!")
    }
  }

  private def lookAhead(tokens:Array[Token], offset:MutableNumber[Int]):Token = {
    if(offset.value >= tokens.length)
      throw new RuntimeException("ERROR: end of stream reached to soon!")
    tokens(offset.value)
  }

  private def tokenizeDis(buffer:String):Array[Token] = {
    val tokens = new ArrayBuffer[Token]()
    var offset = 0
    while(offset < buffer.length) {
      offset = skipWhitespaces(buffer, offset)
      if(offset < buffer.length) {
        // println("TOKENIZING: " + buffer.substring(offset, Math.min(offset + 20, buffer.length)))
        var found = false
        for(pattern <- Reader.TOKENIZATION_PATTERNS if ! found) {
          val m = pattern.pattern.matcher(buffer)
          if(matchesAt(m, offset)) {
            found = true
            var value = ""
            if(pattern.hasValue)
              value = buffer.substring(m.start(1), m.end(1))
            tokens += new Token(pattern.kind, value)
            offset = m.end()
          }
        }
        if(! found) {
          throw new RuntimeException("ERROR: cannot tokenize this text: " +
            buffer.substring(offset, Math.min(offset + 20, buffer.length)) +
            "...")
        }
      }
    }
    tokens.toArray
  }

  private def matchesAt(m:Matcher, offset:Int):Boolean = {
    if(m.find(offset)) {
      if(m.start() == offset) return true
    }
    false
  }

  private def skipWhitespaces(buffer:String, offset:Int):Int = {
    var of = offset
    while(of < buffer.length && Character.isWhitespace(buffer.charAt(of))) of += 1
    of
  }

  /** Reads all .dis files in the given directory */
  def readDir(dirName:String, proc:Processor):List[(DiscourseTree, Document)] = {
    val dir = new File(dirName)
    if(! dir.isDirectory) throw new RuntimeException(s"ERROR: $dirName is not a directory!")
    val trees = new ListBuffer[(DiscourseTree, Document)]
    for(f <- dir.listFiles()){
      if(f.getName.endsWith(".dis")){
        trees += read(f, proc)
      }
    }
    trees.toList
  }
}

class Token (val kind:String, val value:String) {
  override def toString:String = if(value.length > 0) kind + ":" + value else kind
}

class TokenPattern(val pattern:Pattern, val kind:String, val hasValue:Boolean)

object Reader {
  val logger = LoggerFactory.getLogger(classOf[Reader])

  val TOKENIZATION_PATTERNS = Array[TokenPattern](
    new TokenPattern(Pattern.compile("\\(text\\s+_!(.+?)_!\\)", Pattern.CASE_INSENSITIVE), "TEXT", true),
    new TokenPattern(Pattern.compile("\\(rel2par\\s+([a-z\\-A-Z]*)\\)", Pattern.CASE_INSENSITIVE), "LABEL", true),
    new TokenPattern(Pattern.compile("\\(span\\s+[0-9]+\\s+[0-9]+\\)", Pattern.CASE_INSENSITIVE), "SPAN", false),
    new TokenPattern(Pattern.compile("\\(leaf\\s+[0-9]+\\)", Pattern.CASE_INSENSITIVE), "LEAF", false),
    new TokenPattern(Pattern.compile("\\(leaf\\s+[0-9]+\\)", Pattern.CASE_INSENSITIVE), "LEAF", false),
    new TokenPattern(Pattern.compile("(root)", Pattern.CASE_INSENSITIVE), "KIND", true),
    new TokenPattern(Pattern.compile("(nucleus)", Pattern.CASE_INSENSITIVE), "KIND", true),
    new TokenPattern(Pattern.compile("(satellite)", Pattern.CASE_INSENSITIVE), "KIND", true),
    new TokenPattern(Pattern.compile("\\(", Pattern.CASE_INSENSITIVE), "LP", false),
    new TokenPattern(Pattern.compile("\\)", Pattern.CASE_INSENSITIVE), "RP", false)
  )

  val LABEL_PATTERNS = Array[(Pattern, String)](
    (Pattern.compile("attribution", Pattern.CASE_INSENSITIVE), "attribution"),
    (Pattern.compile("background|circumstance", Pattern.CASE_INSENSITIVE), "background"),
    (Pattern.compile("cause|result|consequence", Pattern.CASE_INSENSITIVE), "cause"),
    (Pattern.compile("comparison|preference|analogy|proportion", Pattern.CASE_INSENSITIVE), "comparison"),
    (Pattern.compile("condition|hypothetical|contingency|otherwise", Pattern.CASE_INSENSITIVE), "condition"),
    (Pattern.compile("contrast|concession|antithesis", Pattern.CASE_INSENSITIVE), "contrast"),
    (Pattern.compile("elaboration|example|definition", Pattern.CASE_INSENSITIVE), "elaboration"),
    (Pattern.compile("purpose|enablement", Pattern.CASE_INSENSITIVE), "enablement"),
    (Pattern.compile("evaluation|interpretation|conclusion|comment", Pattern.CASE_INSENSITIVE), "evaluation"),
    (Pattern.compile("evidence|explanation|reason", Pattern.CASE_INSENSITIVE), "explanation"),
    (Pattern.compile("list|disjunction", Pattern.CASE_INSENSITIVE), "joint"),
    (Pattern.compile("manner|means", Pattern.CASE_INSENSITIVE), "manner-means"),
    (Pattern.compile("problem\\-solution|question\\-answer|statement\\-response|topic\\-comment|comment\\-topic|rhetorical\\-question", Pattern.CASE_INSENSITIVE), "topic-comment"),
    (Pattern.compile("summary|restatement", Pattern.CASE_INSENSITIVE), "summary"),
    (Pattern.compile("temporal|sequence|inverted\\-sequence", Pattern.CASE_INSENSITIVE), "temporal"),
    (Pattern.compile("topic\\-shift|topic\\-drift", Pattern.CASE_INSENSITIVE), "topic-change"),
    (Pattern.compile("textualorganization", Pattern.CASE_INSENSITIVE), "textual-organization"),
    (Pattern.compile("same\\-unit", Pattern.CASE_INSENSITIVE), "same-unit")
  )

  val EOS = Pattern.compile("[\\.|\\?|!]+(\\s|\"|'|`|\\))*", Pattern.CASE_INSENSITIVE)
  val P = Pattern.compile("<P>", Pattern.CASE_INSENSITIVE)
  val TT_ERR = Pattern.compile("//\\s*TT_ERR", Pattern.CASE_INSENSITIVE)

  def main(args:Array[String]) {
    val reader = new Reader
    val proc = new FastNLPProcessor()
    //val proc = new CoreNLPProcessor()
    val top = new File(args(0))
    if(top.isDirectory) {
      for(f <- top.listFiles()){
        if(f.getName.endsWith(".dis")){
          println("Parsing file " + f)
          val p = reader.read(f, proc, simplifyRelationLabels=true, verbose=true)
          println(p._1)
        }
      }
    } else {
      val p = reader.read(top, proc, simplifyRelationLabels=true, verbose=true)
      println(p._1)
    }
  }
}
