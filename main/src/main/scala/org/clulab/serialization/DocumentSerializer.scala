package org.clulab.serialization

import java.io._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag
import com.typesafe.scalalogging.LazyLogging
import org.clulab.processors.DocumentAttachment
import org.clulab.processors.DocumentAttachmentBuilderFromText
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct._

/**
  * Saves/loads a Document to/from a stream
  * An important focus here is to minimize the size of the serialized Document.
  * For this reason, we use a custom (compact) text format, rather than XML.
  * User: mihais
  * Date: 3/5/13
  * Last Modified: Don't save zero-length text.
  * Last Modified 06/10/18: Add .raw in sentence serialization
  */
class DocumentSerializer extends LazyLogging {

  import DocumentSerializer._

  /**
   * This is deprecated! Please use load(r:BufferedReader) instead!
   * This does not work correctly when multiple documents are serialized to the same file; load(r:BufferedReader) does.
   **/
  @deprecated ("This doesn't work when there are multiple docs serialized in the same file, sequentially", "4.0")
  def load(is:InputStream): Document = {
    val r = new BufferedReader(new InputStreamReader(is))
    load(r)
  }

  def load (r:BufferedReader): Document = {
    var bits:Array[String] = null
    try {
      bits = read(r)
    } catch {
      case e:NullPointerException => return null // reached the end of stream
      case e:Exception => throw e // something else bad
    }

    assert(bits(0) == START_SENTENCES, s"START_SENTENCES expected, found ${bits(0)}")
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
    } while(bits(0) != END_OF_DOCUMENT && bits(0) != START_DISCOURSE && bits(0) != START_TEXT && bits(0) != START_ATTACHMENTS)

    var text: Option[String] = None
    if (bits(0) == START_TEXT) {
      if (bits.length != 2)
        throw new RuntimeException(
          s"ERROR: Missing text length in start text line: " + bits.mkString(" "))
      val charCount = bits(1).toInt
      text = Some(loadText(r, charCount))
      bits = read(r)
    }

    var namedDocumentAttachmentsOpt: Option[Array[(String, DocumentAttachment)]] = None
    if (bits(0) == START_ATTACHMENTS) {
      def unescapeAttachment(text: String): String = {
        text
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\\\", "\\")
      }

      if (bits.length != 2)
        throw new RuntimeException(
          s"ERROR: Missing document attachments size in start attachments line: " + bits.mkString(" "))
      val attachmentCount = bits(1).toInt
      if (attachmentCount > 0)
        namedDocumentAttachmentsOpt = Some(new Array(attachmentCount))
      0.until(attachmentCount).foreach { index =>
        bits = read(r)
        val key = unescapeAttachment(bits(0))
        val documentAttachmentBuilderFromTextClassName = unescapeAttachment(bits(1))
        // See https://stackoverflow.com/questions/6094575/creating-an-instance-using-the-class-name-and-calling-constructor/6094602
        val clazz = Class.forName(documentAttachmentBuilderFromTextClassName)
        val ctor = clazz.getConstructor()
        val obj = ctor.newInstance()
        val documentAttachmentBuilder = obj.asInstanceOf[DocumentAttachmentBuilderFromText]
        val text = unescapeAttachment(bits(2))
        val documentAttachment = documentAttachmentBuilder.mkDocumentAttachment(text)
        namedDocumentAttachmentsOpt.get(index) = (key, documentAttachment)
      }
      bits = read(r)
    }

    assert(bits(0) == END_OF_DOCUMENT, s"END_OF_DOCUMENT expected, found ${bits(0)}")

    val doc = Document(sents.toArray)
    doc.coreferenceChains = coref
    doc.text = text


    // TODO: Hack by Enrique to resolve the document object for the relations
    for(sen <- doc.sentences){
      sen.relations match {
        case Some(relations) =>
          val newRelations = relations.map(r => RelationTriple(r.confidence, r.subjectInterval, r.relationInterval, r.objectInterval))
          sen.relations = Some(newRelations)
        case None => ()
      }
    }

    namedDocumentAttachmentsOpt.foreach { namedDocumentAttachments =>
      namedDocumentAttachments.foreach { case (name: String, documentAttachment: DocumentAttachment) =>
        doc.addAttachment(name, documentAttachment)
      }
    }

    doc
  }

  private def read(r:BufferedReader, howManyTokens:Int = 0): Array[String] = {
    val line = r.readLine()
    // println("READ LINE: [" + line + "]")
    if (line.isEmpty) return new Array[String](0)
    line.split(SEP, howManyTokens)
  }

  def load(s:String, encoding:String = "UTF-8"): Document = {
    val is = new ByteArrayInputStream(s.getBytes(encoding))
    val r = new BufferedReader(new InputStreamReader(is))
    val doc = load(r)
    r.close()
    doc
  }

  private def loadText (r:BufferedReader, charCount:Int): String = {
    if (charCount < 1) return ""            // sanity check
    val buffer = new Array[Char](charCount)
    r.read(buffer, 0, charCount)
    r.skip(OS_INDEPENDENT_LINE_SEPARATOR.length) // skip over last line separator
    new String(buffer)
  }

  private def mkRelationInterval(s: String) = {
    val t = s.split("-").map(_.toInt)
    assert(t.length == 2)
    Interval(t(0), t(1))
  }

  private def loadRelations(r: BufferedReader, sz: Int):Option[Array[RelationTriple]] = {
    val ret = (0 until sz) map {
      _ =>
        val line = r.readLine()
        val tokens = line.split(SEP)
        val relInterval = tokens(2) match { case "N" => None; case s => Some(mkRelationInterval(s)) }
        RelationTriple(tokens(0).toFloat, mkRelationInterval(tokens(1)), relInterval, mkRelationInterval(tokens(3)))
    }
    Some(ret.toArray)
  }

  private def loadSentence(r:BufferedReader): Sentence = {
    var bits = read(r)
    assert(bits(0) == START_TOKENS, s"START_TOKENS expected, found ${bits(0)}")
    val tokenCount = bits(1).toInt
    val rawBuffer = new ArrayBuffer[String]()
    val startOffsetBuffer = new ArrayBuffer[Int]
    val endOffsetBuffer = new ArrayBuffer[Int]
    val wordBuffer = new ArrayBuffer[String]
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

      if (bits.length != 9) {
        throw new RuntimeException("ERROR: invalid line: " + bits.mkString(" "))
      }

      rawBuffer += bits(0)
      startOffsetBuffer += bits(1).toInt
      endOffsetBuffer += bits(2).toInt
      wordBuffer += bits(3)

      tagBuffer += bits(4)
      if (bits(4) != NIL) nilTags = false
      lemmaBuffer += bits(5)
      if (bits(5) != NIL) nilLemmas = false
      entityBuffer += bits(6)
      if (bits(6) != NIL) nilEntities = false
      normBuffer += bits(7)
      if (bits(6) != NIL) nilNorms = false
      chunkBuffer += bits(8)
      if (bits(8) != NIL) nilChunks = false
      offset += 1
    }
    assert(rawBuffer.size == tokenCount)
    assert(wordBuffer.size == tokenCount)
    assert(startOffsetBuffer.size == tokenCount)
    assert(endOffsetBuffer.size == tokenCount)
    assert(tagBuffer.isEmpty || tagBuffer.size == tokenCount)
    assert(lemmaBuffer.isEmpty || lemmaBuffer.size == tokenCount)
    assert(entityBuffer.isEmpty || entityBuffer.size == tokenCount)
    assert(normBuffer.isEmpty || normBuffer.size == tokenCount)
    assert(chunkBuffer.isEmpty || chunkBuffer.size == tokenCount)

    var deps = new GraphMap
    var tree:Option[Tree] = None
    var relations:Option[Array[RelationTriple]] = None
    do {
      bits = read(r)
      if (bits(0) == START_DEPENDENCIES) {
        val dt = bits(1)
        val sz = bits(2).toInt
        val d = loadDependencies(r, sz)
        deps += (dt -> d)
      } else if (bits(0) == START_CONSTITUENTS) {
        val position = new MutableNumber[Int](0)
        bits = read(r)
        tree = Some(loadTree(bits, position))
      } else if (bits(0) == START_RELATIONS) {
        val sz = bits(1).toInt
        relations = loadRelations(r, sz)
      }
    } while(bits(0) != END_OF_SENTENCE)

    Sentence(
      rawBuffer.toArray,
      startOffsetBuffer.toArray,
      endOffsetBuffer.toArray,
      wordBuffer.toArray,
      bufferOption(tagBuffer, nilTags),
      bufferOption(lemmaBuffer, nilLemmas),
      bufferOption(entityBuffer, nilEntities),
      bufferOption(normBuffer, nilNorms),
      bufferOption(chunkBuffer, nilChunks),
      tree, deps, relations
    )
  }

  private def loadDependencies(r:BufferedReader, sz:Int):DirectedGraph[String] = {
    val edges = new ListBuffer[Edge[String]]
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
        val edge = Edge(source = bits(0).toInt, destination = bits(1).toInt, relation = bits(2))
        //println("adding edge: " + edge)
        edges += edge
      }
    } while(bits(0) != END_OF_DEPENDENCIES)
    val dg = new DirectedGraph[String](edges.toList, Some(sz))
    //println(dg)
    dg
  }

  private def bufferOption[T: ClassTag](b:ArrayBuffer[T], allNils:Boolean): Option[Array[T]] = {
    if (b.isEmpty) return None
    if (allNils) return None
    Some(b.toArray)
  }

  def save(doc:Document, os:PrintWriter): Unit = save(doc, os, keepText = false)

  def save(doc:Document, os:PrintWriter, keepText:Boolean): Unit = {
    os.println(START_SENTENCES + SEP + doc.sentences.length)
    for (s <- doc.sentences) {
      saveSentence(s, os)
    }
    if (doc.coreferenceChains.nonEmpty) {
      val mentionCount = doc.coreferenceChains.get.getMentions.size
      os.println(START_COREF + SEP + mentionCount)
      doc.coreferenceChains.foreach(g => saveCoref(g, os))
    }

    if (keepText && doc.text.nonEmpty) {
      val txtLen = doc.text.get.length
      if (txtLen > 0) {
        os.println(START_TEXT + SEP + txtLen)
        // Do not add OS-specific separator with println in case client and server use different conventions.
        os.print(doc.text.get)                 
        // Instead, use a separator that is independent of operating system.
        // Or at the very least, use system property line.separator to account for println.
        // See loadText which must be coordinated with this decision.
        os.print(OS_INDEPENDENT_LINE_SEPARATOR)
      }
    }

    {
      def escapeAttachment(text: String): String = {
        text
            .replace("\\", "\\\\")
            .replace("\t", "\\t")
            .replace("\n", "\\n")
      }

      // Sort these so that serialization is the same each time.
      val attachmentKeys = doc.getAttachmentKeys.toList.sorted
      if (attachmentKeys.nonEmpty) {
        os.println(START_ATTACHMENTS + SEP + attachmentKeys.size)
        attachmentKeys.foreach { key =>
          val value = doc.getAttachment(key).get
          os.print(escapeAttachment(key))
          os.print(SEP)
          os.print(escapeAttachment(value.documentAttachmentBuilderFromTextClassName))
          os.print(SEP)
          os.println(escapeAttachment(value.toDocumentSerializer))
        }
      }
    }

    os.println(END_OF_DOCUMENT)
  }

  def save(doc:Document, encoding:String = "UTF-8", keepText:Boolean = false): String = {
    val byteOutput = new ByteArrayOutputStream
    val os = new PrintWriter(byteOutput)
    save(doc, os, keepText)
    os.flush()
    os.close()
    byteOutput.close()
    byteOutput.toString(encoding)
  }

  private def saveSentence(sent:Sentence, os:PrintWriter): Unit = {
    os.println(START_TOKENS + SEP + sent.size)
    var offset = 0
    while(offset < sent.size) {
      saveToken(sent, offset, os)
      offset += 1
    }
    if (sent.graphs.nonEmpty) {
      for(t <- sent.graphs.keySet) {
        saveDependencies(sent.graphs(t), t, os)
      }
    }
    if (sent.syntacticTree.nonEmpty) {
      os.println(START_CONSTITUENTS + SEP + "1")
      sent.syntacticTree.foreach(t => { saveTree(t, os); os.println() })
    }
    if (sent.relations.nonEmpty) {
      val relations = sent.relations.get
      os.println(START_RELATIONS + SEP + relations.length)
      relations foreach (t => saveRelationTriple(t, os))
    }
    os.println(END_OF_SENTENCE)
  }

  private def saveTree(tree:Tree, os:PrintWriter): Unit = {
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

    val n = NonTerminal(value, children)
    n.setStartEndIndices(startOffset, endOffset)
    n.setHead(head)
    n
    // new Tree[String](value, Some(children), head, startOffset, endOffset)
  }

  private def saveRelationTriple(t:RelationTriple, os:PrintWriter): Unit = {
    os.print(s"${t.confidence}$SEP${t.subjectInterval.start}-${t.subjectInterval.end}$SEP")
    t.relationInterval match {
      case Some(i) =>
        os.print(s"${i.start}-${i.end}")
      case None =>
        os.print("N")
    }

    os.println(s"$SEP${t.objectInterval.start}-${t.objectInterval.end}")
  }

  private def saveToken(sent:Sentence, offset:Int, os:PrintWriter): Unit = {
    os.print(sent.raw(offset) + SEP +
      sent.startOffsets(offset) + SEP +
      sent.endOffsets(offset) + SEP +
      sent.words(offset))

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

  private def saveDependencies(dg: DirectedGraph[String], dependencyType: String, os: PrintWriter): Unit = {
    os.println(START_DEPENDENCIES + SEP + dependencyType + SEP + dg.size)
    // For consistent output, contents of sets must be sorted.
    os.println(dg.roots.toSeq.sorted.mkString(sep = SEP))
    val it = new DirectedGraphEdgeIterator[String](dg)
    while(it.hasNext) {
      val edge = it.next()
      os.println(edge._1 + SEP + edge._2 + SEP + edge._3)
    }
    os.println(END_OF_DEPENDENCIES)
  }

  private def saveCoref(cg:CorefChains, os:PrintWriter): Unit = {
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
      mb += CorefMention(
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
  protected val OS_INDEPENDENT_LINE_SEPARATOR = "\n"

  val NIL = "_"
  val SEP = "\t"

  val START_SENTENCES = "S"
  val START_TEXT = "TX"
  val START_TOKENS = "T"
  val START_COREF = "C"
  val START_DEPENDENCIES = "D"
  val START_CONSTITUENTS = "Y"
  val START_DISCOURSE = "R"
  val START_RELATIONS = "OIE"
  val START_ATTACHMENTS = "A"

  val END_OF_SENTENCE = "EOS"
  val END_OF_DOCUMENT = "EOD"
  val END_OF_DEPENDENCIES = "EOX"
}
