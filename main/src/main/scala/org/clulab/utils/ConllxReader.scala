package org.clulab.utils

import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.{DirectedGraph, Edge, GraphMap}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.annotation.tailrec
import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8


trait conll {
  val NIL = "_"
  val SEP = "\t"
  val NUM_COLS = 10
  val READ_AHEAD_LIMIT = 300

  val ID = 0
  val FORM = 1
  val LEMMA = 2
  val CPOSTAG = 3
  val POSTAG = 4
  val FEATS = 5
  val HEAD = 6
  val DEPREL = 7
  val PHEAD = 8
  val PDEPREL = 9
}

/**
  * Read a CoNLL-X file
  */
object ConllxReader extends conll {

  def load(f: File, charset: Charset = UTF_8): Document = {
    val br = new BufferedReader(new InputStreamReader(new FileInputStream(f), charset))
    val doc = load(br)
    br.close()
    doc
  }

  def load(br: BufferedReader): Document = {
    val sents = loadSentences(br)
    Document(sents.toArray)
  }

  def loadSentences(br: BufferedReader): Seq[Sentence] = {
    @tailrec
    def getSentences(br: BufferedReader, sentences: Seq[Sentence] = Nil): Seq[Sentence] = loadSentence(br) match {
      case None => sentences
      case Some(s) => getSentences(br, sentences ++ Seq(s))
    }
    val ss = getSentences(br)
    br.close()
    ss
  }

  // adapted from main/org.clulab.serialization.DocumentSerializer
  private def loadSentence(br: BufferedReader): Option[Sentence] = {
    // Ensure there is something to read
    br.mark(READ_AHEAD_LIMIT)
    if (br.readLine == null) return None
    br.reset

    val wordBuffer = new ArrayBuffer[String]
    val startOffsetBuffer = new ArrayBuffer[Int]
    val endOffsetBuffer = new ArrayBuffer[Int]
    val tagBuffer = new ArrayBuffer[String]
    val lemmaBuffer = new ArrayBuffer[String]

    var line:String = null
    var tokenCount = 0

    var deps = new GraphMap
    val edges = new ListBuffer[Edge[String]]
    val roots = new mutable.HashSet[Int]()

    var offset = 0

    while({line = br.readLine; line != null && line != ""}) {
      val bits = line.split(SEP)

      if (bits.length != NUM_COLS) {
        throw new RuntimeException("ERROR: invalid line: " + bits.mkString(" "))
      }

      assert(bits.length == NUM_COLS)

      startOffsetBuffer += offset
      offset += bits(FORM).length + 1 // include a space
      endOffsetBuffer += offset - 1

      wordBuffer += bits(FORM)
      lemmaBuffer += bits(LEMMA)
      tagBuffer += bits(POSTAG) // fine-grained POS tag (coarse-grained is column 3)


      // Only create edges for non-roots
      if (bits(HEAD).toInt != 0) {
        val edge = Edge(source = bits(HEAD).toInt-1, destination = bits(ID).toInt-1, relation = bits(DEPREL))
        //        println("adding edge: " + edge)
        edges += edge
      }
      else roots.add(bits(ID).toInt-1)

      tokenCount += 1
    }

    assert(wordBuffer.size == tokenCount)
    assert(startOffsetBuffer.size == tokenCount)
    assert(endOffsetBuffer.size == tokenCount)
    assert(tagBuffer.isEmpty || tagBuffer.size == tokenCount)
    assert(lemmaBuffer.isEmpty || lemmaBuffer.size == tokenCount)

    val dg = new DirectedGraph[String](edges.toList, roots.toSet)
    deps += (GraphMap.STANFORD_BASIC -> dg)
    //println(dg)

    val s = Sentence(
      wordBuffer.toArray,
      startOffsetBuffer.toArray,
      endOffsetBuffer.toArray,
      Some(tagBuffer.toArray),
      Some(lemmaBuffer.toArray),
      None, // entities
      None, // norms
      None, // chunks
      None, // tree
      deps
    )
    Some(s)
  }
}
