package edu.arizona.sista.processors

import java.io._
import edu.arizona.sista.processors.DocumentSerializer._
import edu.arizona.sista.struct.{Tree, MutableNumber, DirectedGraphEdgeIterator, DirectedGraph}
import collection.mutable.{ListBuffer, ArrayBuffer}
import collection.mutable

/**
 * Saves/loads a Document to/from a stream
 * An important focus here is to minimize the size of the serialized Document.
 * For this reason, we use a custom (compact) text format, rather than XML.
 * User: mihais
 * Date: 3/5/13
 */
class DocumentSerializer {

  def load(is:InputStream): Document = {
    val r = new BufferedReader(new InputStreamReader(is))
    var bits = read(r)
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
    } while(bits(0) != END_OF_DOCUMENT)

    // TODO: load discourse tree

    new Document(sents.toArray, coref, None)
  }

  private def read(r:BufferedReader): Array[String] = {
    val line = r.readLine()
    // println("READ LINE: [" + line + "]")
    if (line.length == 0) return new Array[String](0)
    line.split(SEP)
  }

  def load(s:String, encoding:String = "ISO-8859-1"): Document = {
    // println("Parsing annotation:\n" + s)
    val is = new ByteArrayInputStream(s.getBytes(encoding))
    val doc = load(is)
    is.close()
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
    assert(tagBuffer.size == 0 || tagBuffer.size == tokenCount)
    assert(lemmaBuffer.size == 0 || lemmaBuffer.size == tokenCount)
    assert(entityBuffer.size == 0 || entityBuffer.size == tokenCount)
    assert(normBuffer.size == 0 || normBuffer.size == tokenCount)
    assert(chunkBuffer.size == 0 || chunkBuffer.size == tokenCount)

    var deps:Option[DirectedGraph[String]] = None
    var tree:Option[Tree[String]] = None
    do {
      bits = read(r)
      if (bits(0) == START_DEPENDENCIES) {
        deps = Some(loadDependencies(r))
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

  private def loadDependencies(r:BufferedReader):DirectedGraph[String] = {
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

  private def bufferOption[T: ClassManifest](b:ArrayBuffer[T], allNils:Boolean): Option[Array[T]] = {
    if (b.size == 0) return None
    if (allNils) return None
    Some(b.toArray)
  }

  def save(doc:Document, os:PrintWriter) {
    os.println(START_SENTENCES + SEP + doc.sentences.length)
    for (s <- doc.sentences) {
      saveSentence(s, os)
    }
    if (! doc.coreferenceChains.isEmpty) {
      val mentionCount = doc.coreferenceChains.get.getMentions.size
      os.println(START_COREF + SEP + mentionCount)
      doc.coreferenceChains.foreach(g => saveCoref(g, os))
    }

    // TODO: save discourse tree

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
    if (! sent.dependencies.isEmpty) {
      os.println(START_DEPENDENCIES + SEP + sent.dependencies.size)
      sent.dependencies.foreach(g => saveDependencies(g, os))
    }
    if (! sent.syntacticTree.isEmpty) {
      os.println(START_CONSTITUENTS + SEP + "1")
      sent.syntacticTree.foreach(t => { saveTree(t, os); os.println() })
    }
    os.println(END_OF_SENTENCE)
  }

  private def saveTree(tree:Tree[String], os:PrintWriter) {
    os.print(tree.value + SEP + tree.head + SEP + tree.startOffset + SEP + tree.endOffset + SEP)
    if (tree.children == None) os.print(0)
    else os.print(tree.children.get.length)
    if (! tree.isLeaf) {
      for(c <- tree.children.get) {
        os.print(SEP)
        saveTree(c, os)
      }
    }
  }

  private def loadTree(bits:Array[String], position:MutableNumber[Int]):Tree[String] = {
    val value = bits(position.value)
    val head = bits(position.value + 1).toInt
    val startOffset = bits(position.value + 2).toInt
    val endOffset = bits(position.value + 3).toInt
    val numChildren = bits(position.value + 4).toInt
    position.value += 5

    if (numChildren == 0) {
      return new Tree[String](value, None, head, startOffset, endOffset)
    }

    val children = new Array[Tree[String]](numChildren)
    for (i <- 0 until numChildren) {
      children(i) = loadTree(bits, position)
    }

    new Tree[String](value, Some(children), head, startOffset, endOffset)
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

  private def saveDependencies(dg:DirectedGraph[String], os:PrintWriter) {
    os.println(dg.roots.mkString(sep = SEP))
    val it = new DirectedGraphEdgeIterator[String](dg)
    while(it.hasNext) {
      val edge = it.next()
      os.println(edge._1 + SEP + edge._2 + SEP + edge._3)
    }
    os.println(END_OF_DEPENDENCIES)
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

  val END_OF_SENTENCE = "EOS"
  val END_OF_DOCUMENT = "EOD"
  val END_OF_DEPENDENCIES = "EOX"
}
