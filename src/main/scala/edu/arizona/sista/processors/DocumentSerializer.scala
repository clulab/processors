package edu.arizona.sista.processors

import java.io._
import edu.arizona.sista.discourse.rstparser.{TreeKind, TokenOffset, RelationDirection, DiscourseTree}
import edu.arizona.sista.processors.DocumentSerializer._
import edu.arizona.sista.struct._
import collection.mutable.{ListBuffer, ArrayBuffer}
import collection.mutable
import scala.reflect.ClassTag

/**
 * Saves/loads a Document to/from a stream
 * An important focus here is to minimize the size of the serialized Document.
 * For this reason, we use a custom (compact) text format, rather than XML.
 * User: mihais
 * Date: 3/5/13
 */
class DocumentSerializer {

  /**
   * This is deprecated! Please use load(r:BufferedReader) instead!
   * This does not work correctly when multiple documents are serialized to the same file; load(r:BufferedReader) does.
   **/
  @deprecated ("This doesn't work when there are multiple docs serialized in the same file, sequentially", "4.0")
  def load(is:InputStream): Document = {
    val r = new BufferedReader(new InputStreamReader(is))
    load(r)
  }

  def load(r:BufferedReader): Document = {
    var bits:Array[String] = null
    try {
      bits = read(r)
    } catch {
      case e:NullPointerException => return null // reached the end of stream
      case e:Exception => throw e // something else bad
    }
    assert(bits(0) == START_SENTENCES)
    val sentCount = bits(1).toInt
    val sents = new ArrayBuffer[Sentence]
    var offset = 0
    while(offset < sentCount) {
      sents += loadSentence(r)
      offset += 1
    }
    var coref:Option[CorefChains] = None
    do {
      bits = read(r)
      if (bits(0) == START_COREF) {
        coref = Some(loadCoref(r, bits(1).toInt))
      }
    } while(bits(0) != END_OF_DOCUMENT && bits(0) != START_DISCOURSE)

    var discourse:Option[DiscourseTree] = None
    if(bits(0) == START_DISCOURSE) {
      discourse = Some(loadDiscourse(r))
      bits = read(r)
      assert(bits(0) == END_OF_DOCUMENT)
    }

    new Document(sents.toArray, coref, discourse)
  }

  private def read(r:BufferedReader, howManyTokens:Int = 0): Array[String] = {
    val line = r.readLine()
    // println("READ LINE: [" + line + "]")
    if (line.length == 0) return new Array[String](0)
    line.split(SEP, howManyTokens)
  }

  def load(s:String, encoding:String = "ISO-8859-1"): Document = {
    // println("Parsing annotation:\n" + s)
    val is = new ByteArrayInputStream(s.getBytes(encoding))
    val r = new BufferedReader(new InputStreamReader(is))
    val doc = load(r)
    r.close()
    doc
  }

  private def loadSentence(r:BufferedReader): Sentence = {
    var bits = read(r)
    assert(bits(0) == START_TOKENS)
    val tokenCount = bits(1).toInt
    val wordBuffer = new ArrayBuffer[String]
    val startOffsetBuffer = new ArrayBuffer[Int]
    val endOffsetBuffer = new ArrayBuffer[Int]
    val tagBuffer = new ArrayBuffer[String]
    var nilTags = true
    val lemmaBuffer = new ArrayBuffer[String]
    var nilLemmas = true
    val entityBuffer = new ArrayBuffer[String]
    var nilEntities = true
    val normBuffer = new ArrayBuffer[String]
    var nilNorms = true
    val chunkBuffer = new ArrayBuffer[String]
    var nilChunks = true
    var offset = 0
    while(offset < tokenCount) {
      bits = read(r)

      if(bits.length != 8) {
        throw new RuntimeException("ERROR: invalid line: " + bits.mkString(" "))
      }

      assert(bits.length == 8)
      wordBuffer += bits(0)
      startOffsetBuffer += bits(1).toInt
      endOffsetBuffer += bits(2).toInt

      tagBuffer += bits(3)
      if (bits(3) != NIL) nilTags = false
      lemmaBuffer += bits(4)
      if (bits(4) != NIL) nilLemmas = false
      entityBuffer += bits(5)
      if (bits(5) != NIL) nilEntities = false
      normBuffer += bits(6)
      if (bits(6) != NIL) nilNorms = false
      chunkBuffer += bits(7)
      if (bits(7) != NIL) nilChunks = false
      offset += 1
    }
    assert(wordBuffer.size == tokenCount)
    assert(startOffsetBuffer.size == tokenCount)
    assert(endOffsetBuffer.size == tokenCount)
    assert(tagBuffer.isEmpty || tagBuffer.size == tokenCount)
    assert(lemmaBuffer.isEmpty || lemmaBuffer.size == tokenCount)
    assert(entityBuffer.isEmpty || entityBuffer.size == tokenCount)
    assert(normBuffer.isEmpty || normBuffer.size == tokenCount)
    assert(chunkBuffer.isEmpty || chunkBuffer.size == tokenCount)

    var deps = new DependencyMap
    var tree:Option[Tree] = None
    do {
      bits = read(r)
      if (bits(0) == START_DEPENDENCIES) {
        val dt = bits(1).toInt
        val sz = bits(2).toInt
        val d = loadDependencies(r, sz)
        deps += (dt -> d)
      } else if (bits(0) == START_CONSTITUENTS) {
        val position = new MutableNumber[Int](0)
        bits = read(r)
        tree = Some(loadTree(bits, position))
      }
    } while(bits(0) != END_OF_SENTENCE)

    new Sentence(
      wordBuffer.toArray,
      startOffsetBuffer.toArray,
      endOffsetBuffer.toArray,
      bufferOption(tagBuffer, nilTags),
      bufferOption(lemmaBuffer, nilLemmas),
      bufferOption(entityBuffer, nilEntities),
      bufferOption(normBuffer, nilNorms),
      bufferOption(chunkBuffer, nilChunks),
      tree, deps
    )
  }

  private def loadDependencies(r:BufferedReader, sz:Int):DirectedGraph[String] = {
    val edges = new ListBuffer[(Int, Int, String)]
    val roots = new mutable.HashSet[Int]()
    var bits = read(r)
    var offset = 0
    while(offset < bits.length) {
      roots.add(bits(offset).toInt)
      offset += 1
    }
    do {
      bits = read(r)
      if (bits(0) != END_OF_DEPENDENCIES) {
        val edge = new Tuple3(bits(0).toInt, bits(1).toInt, bits(2))
        //println("adding edge: " + edge)
        edges += edge
      }
    } while(bits(0) != END_OF_DEPENDENCIES)
    val dg = new DirectedGraph[String](edges.toList, roots.toSet)
    //println(dg)
    dg
  }

  private def bufferOption[T: ClassTag](b:ArrayBuffer[T], allNils:Boolean): Option[Array[T]] = {
    if (b.isEmpty) return None
    if (allNils) return None
    Some(b.toArray)
  }

  def save(doc:Document, os:PrintWriter) {
    os.println(START_SENTENCES + SEP + doc.sentences.length)
    for (s <- doc.sentences) {
      saveSentence(s, os)
    }
    if (doc.coreferenceChains.nonEmpty) {
      val mentionCount = doc.coreferenceChains.get.getMentions.size
      os.println(START_COREF + SEP + mentionCount)
      doc.coreferenceChains.foreach(g => saveCoref(g, os))
    }

    if(doc.discourseTree.nonEmpty) {
      os.println(START_DISCOURSE)
      doc.discourseTree.foreach(d => saveDiscourse(d, os))
    }

    os.println(END_OF_DOCUMENT)
  }

  def save(doc:Document, encoding:String = "ISO-8859-1"): String = {
    val byteOutput = new ByteArrayOutputStream
    val os = new PrintWriter(byteOutput)
    save(doc, os)
    os.flush()
    os.close()
    byteOutput.close()
    byteOutput.toString(encoding)
  }

  private def saveSentence(sent:Sentence, os:PrintWriter) {
    os.println(START_TOKENS + SEP + sent.size)
    var offset = 0
    while(offset < sent.size) {
      saveToken(sent, offset, os)
      offset += 1
    }
    if (sent.dependenciesByType.nonEmpty) {
      for(t <- sent.dependenciesByType.keySet) {
        saveDependencies(sent.dependenciesByType.get(t).get, t, os)
      }
    }
    if (sent.syntacticTree.nonEmpty) {
      os.println(START_CONSTITUENTS + SEP + "1")
      sent.syntacticTree.foreach(t => { saveTree(t, os); os.println() })
    }
    os.println(END_OF_SENTENCE)
  }

  private def saveTree(tree:Tree, os:PrintWriter) {
    os.print(tree.value + SEP + tree.head + SEP + tree.startOffset + SEP + tree.endOffset + SEP)
    if (tree.children.isEmpty) os.print(0)
    else os.print(tree.children.get.length)
    if (! tree.isLeaf) {
      for(c <- tree.children.get) {
        os.print(SEP)
        saveTree(c, os)
      }
    }
  }

  private def loadTree(bits:Array[String], position:MutableNumber[Int]):Tree = {
    val value = bits(position.value)
    val head = bits(position.value + 1).toInt
    val startOffset = bits(position.value + 2).toInt
    val endOffset = bits(position.value + 3).toInt
    val numChildren = bits(position.value + 4).toInt
    position.value += 5

    if (numChildren == 0) {
      val t = Terminal(value)
      t.setIndex(startOffset)
      return t
      // return new Tree[String](value, None, head, startOffset, endOffset)
    }

    val children = new Array[Tree](numChildren)
    for (i <- 0 until numChildren) {
      children(i) = loadTree(bits, position)
    }

    val n = new NonTerminal(value, children)
    n.setStartEndIndices(startOffset, endOffset)
    n.setHead(head)
    n
    // new Tree[String](value, Some(children), head, startOffset, endOffset)
  }

  private def saveToken(sent:Sentence, offset:Int, os:PrintWriter) {
    os.print(sent.words(offset) + SEP +
      sent.startOffsets(offset) + SEP +
      sent.endOffsets(offset))

    os.print(SEP)
    if (sent.tags.isDefined) os.print(sent.tags.get(offset))
    else os.print(NIL)

    os.print(SEP)
    if (sent.lemmas.isDefined) os.print(sent.lemmas.get(offset))
    else os.print(NIL)

    os.print(SEP)
    if (sent.entities.isDefined) os.print(sent.entities.get(offset))
    else os.print(NIL)

    os.print(SEP)
    if (sent.norms.isDefined) os.print(sent.norms.get(offset))
    else os.print(NIL)

    os.print(SEP)
    if (sent.chunks.isDefined) os.print(sent.chunks.get(offset))
    else os.print(NIL)

    os.println()
  }

  private def saveDependencies(dg:DirectedGraph[String], dependencyType:Int, os:PrintWriter) {
    os.println(START_DEPENDENCIES + SEP + dependencyType + SEP + dg.size)
    os.println(dg.roots.mkString(sep = SEP))
    val it = new DirectedGraphEdgeIterator[String](dg)
    while(it.hasNext) {
      val edge = it.next()
      os.println(edge._1 + SEP + edge._2 + SEP + edge._3)
    }
    os.println(END_OF_DEPENDENCIES)
  }

  private def loadDiscourse(r:BufferedReader):DiscourseTree = {
    val bits = read(r, 12)
    if(bits.length != 12)
      throw new RuntimeException(s"ERROR: found ${bits.length} tokens in invalid discourse tree line: " + bits.mkString(" "))
    val label = bits(0) match {
      case "nil" => ""
      case _ => bits(0)
    }
    val dir = bits(1) match {
      case "LeftToRight" => RelationDirection.LeftToRight
      case "RightToLeft" => RelationDirection.RightToLeft
      case "None" => RelationDirection.None
      case _ => throw new RuntimeException("ERROR: unknown relation direction " + bits(1))
    }
    val charOffsets = (bits(2).toInt, bits(3).toInt)
    val firstToken = new TokenOffset(bits(4).toInt, bits(5).toInt)
    val lastToken = new TokenOffset(bits(6).toInt, bits(7).toInt)
    val firstEDU = bits(8).toInt
    val lastEDU = bits(9).toInt
    val childrenCount = bits(10).toInt
    val children:Array[DiscourseTree] = childrenCount match {
      case 0 => null
      case _ => new Array[DiscourseTree](childrenCount)
    }
    val text:String = bits(11) match {
      case "nil" => null
      case _ => bits(11)
    }

    val d = new DiscourseTree(
      label, dir,
      children,
      TreeKind.Nucleus, // not used
      text,
      charOffsets,
      firstToken,
      lastToken,
      firstEDU,
      lastEDU)

    for(i <- 0 until childrenCount) {
      d.children(i) = loadDiscourse(r)
    }

    d
  }

  private def saveDiscourse(d:DiscourseTree, os:PrintWriter) {
    var childrenCount = 0
    if(! d.isTerminal) childrenCount = d.children.length
    var label = d.relationLabel
    if(label == "") label = "nil"
    var text = d.rawText
    if(text == null) text = "nil"
    os.println(s"$label\t${d.relationDirection}\t${d.charOffsets._1}\t${d.charOffsets._2}\t${d.firstToken.sentence}\t${d.firstToken.token}\t${d.lastToken.sentence}\t${d.lastToken.token}\t${d.firstEDU}\t${d.lastEDU}\t$childrenCount\t$text")
    if(childrenCount > 0) {
      for(c <- d.children) {
        saveDiscourse(c, os)
      }
    }
  }

  private def saveCoref(cg:CorefChains, os:PrintWriter) {
    val mentions = cg.getMentions
    for (m <- mentions) {
      os.println(
        m.sentenceIndex + SEP +
        m.headIndex + SEP +
        m.startOffset + SEP +
        m.endOffset + SEP +
        m.chainId)
    }
  }

  private def loadCoref(r:BufferedReader, mentionCount:Int): CorefChains = {
    val mb = new ListBuffer[CorefMention]
    for (i <- 0 until mentionCount) {
      val bits = read(r)
      mb += new CorefMention(
        bits(0).toInt,
        bits(1).toInt,
        bits(2).toInt,
        bits(3).toInt,
        bits(4).toInt)
    }
    new CorefChains(mb.toList)
  }
}

object DocumentSerializer {
  val NIL = "_"
  val SEP = "\t"

  val START_SENTENCES = "S"
  val START_TOKENS = "T"
  val START_COREF = "C"
  val START_DEPENDENCIES = "D"
  val START_CONSTITUENTS = "Y"
  val START_DISCOURSE = "R"

  val END_OF_SENTENCE = "EOS"
  val END_OF_DOCUMENT = "EOD"
  val END_OF_DEPENDENCIES = "EOX"
}
