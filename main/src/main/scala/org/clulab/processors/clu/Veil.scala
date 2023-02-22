package org.clulab.processors.clu

import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.serialization.DocumentSerializer
import org.clulab.struct.{DirectedGraph, Edge, GraphMap, RelationTriple, Tree}
import org.clulab.utils.Closer.AutoCloser

import java.io.PrintWriter
import scala.collection.mutable.{Set => MutableSet}

trait Veil

object Veil {
  val veiledTag    = ""
  val veiledLemma  = ""
  val veiledEntity = ""
  val veiledNorm   = ""
  val veiledChunk  = ""
}

/** Manipulate a document with veiled text
  *
  * @param originalText text that has not yet been veiled
  * @param veiledLetters a sequence of ranges which specify by index which letters in the original text to veil
  *                      when a document is created with mkDocument(processor)
  *
  * See [[VeilApp.veilText]] for an example.
  */
class VeiledText(originalText: String, veiledLetters: Seq[Range]) extends Veil {
  /** This is a set containing veiled letter indices.
    * They have been vetted and deduplicated.
    */
  protected lazy val veilSet: MutableSet[Int] = {
    val set = MutableSet.empty[Int]
    val letterIndexes = originalText.indices

    veiledLetters.foreach { letterRange =>
      letterRange.foreach { letterIndex =>
        letterIndexes.lift(letterIndex).foreach(set += _)
      }
    }
    set
  }
  protected lazy val veiledText: String = {
    val letters = new StringBuffer(originalText)

    veilSet.foreach(letters.setCharAt(_, ' '))
    letters.toString
  }

  protected def unveilDocument(veiledDocument: Document): Document = {
    val unveiledDocument = veiledDocument.copy(textOpt = Some(originalText))

    unveiledDocument
  }

  def mkDocument(processor: Processor): Document = {
    val veiledDocument = processor.mkDocument(veiledText)
    val unveiledDocument = unveilDocument(veiledDocument)

    unveiledDocument
  }
}

/** Manipulate a document with text veiled by word
  *
  * @param originalDocument a document that has not yet been veiled
  * @param veiledWords a sequence of (integer, range) pairs which specify by sentence index and then word index range
  *                    which words of a document to veil during annotation with annotate(processor)
  *
  * See [[VeilApp.veilDocument]] for an example.
  */
class VeiledDocument(originalDocument: Document, veiledWords: Seq[(Int, Range)]) extends Veil {
  /** This is an array of sets, each containing veiled word indices for each sentence.
    * They have been vetted and deduplicated.
    */
  protected lazy val veilSets: Array[MutableSet[Int]] = {
    val sets = Array.fill(originalDocument.sentences.length)(MutableSet.empty[Int])

    veiledWords.foreach { case (sentenceIndex, wordRange) =>
      sets.lift(sentenceIndex).foreach { set =>
        val wordIndexes = originalDocument.sentences(sentenceIndex).words.indices

        wordRange.foreach { wordIndex =>
          wordIndexes.lift(wordIndex).foreach(set += _)
        }
      }
    }
    sets
  }
  /**
    *
    */
  protected lazy val unveilArrays = {
    val arrays = originalDocument.sentences.zip(veilSets).map { case (originalSentence, set) =>
      val array = new Array[Int](originalSentence.words.length - set.size)
      var ununveiledIndex = 0

      array.indices.foreach { veiledIndex =>
        while (set(ununveiledIndex))
          ununveiledIndex += 1
        array(veiledIndex) = ununveiledIndex
        ununveiledIndex += 1
      }
      array
    }

    arrays
  }
  protected lazy val veiledDocument = {
    val veiledSentences = originalDocument.sentences.zipWithIndex.map { case (originalSentence, sentenceIndex) =>
      val wordIndexes = originalSentence.words.indices.filterNot(veilSets(sentenceIndex)).toArray
      val veiledRaw          = wordIndexes.map(originalSentence.raw)
      val veiledStartOffsets = wordIndexes.map(originalSentence.startOffsets)
      val veiledEndOffsets   = wordIndexes.map(originalSentence.endOffsets)
      val veiledWords        = wordIndexes.map(originalSentence.words)
      val veiledSentence = originalSentence.copy(veiledRaw, veiledStartOffsets, veiledEndOffsets, veiledWords)

      veiledSentence
    }

    originalDocument.copy(veiledSentences)
  }

  def unveilStringArray(veiledArrayOpt: Option[Array[String]], sentenceIndex: Int, veil: String): Option[Array[String]] = {
    val unveilArray = unveilArrays(sentenceIndex)
    val originalLength = originalDocument.sentences(sentenceIndex).words.length

    veiledArrayOpt.map { veiledArray =>
      val unveiledArray = Array.fill(originalLength)(veil)

      veiledArray.zipWithIndex.foreach { case (veiledString, veiledIndex) =>
        unveiledArray(unveilArray(veiledIndex)) = veiledString
      }
      unveiledArray
    }
  }

  def unveilGraphs(veiledGraphs: GraphMap, sentenceIndex: Int): GraphMap = {
    val unveilArray = unveilArrays(sentenceIndex)
    val unveiledGraphs = GraphMap()
    val originalLength = originalDocument.sentences(sentenceIndex).words.length

    veiledGraphs.foreach { case (name, veiledDirectedGraph) =>
      val unveiledEdges = veiledDirectedGraph.allEdges.map { case (veiledSource, veiledDestination, relation) =>
        Edge(unveilArray(veiledSource), unveilArray(veiledDestination), relation)
      }
      val unveiledRoots = veiledDirectedGraph.roots.map(unveilArray)

      unveiledGraphs(name) = new DirectedGraph(unveiledEdges, Some(originalLength), Some(unveiledRoots))
    }
    unveiledGraphs
  }

  // TODO
  def unveilSyntacticTree(syntacticTreeOpt: Option[Tree]): Option[Tree] = syntacticTreeOpt

  // TODO
  def unveilRelations(relations: Option[Array[RelationTriple]]): Option[Array[RelationTriple]] = relations

  protected def unveilSentence(veiledSentence: Sentence, sentenceIndex: Int): Sentence = {
    val originalSentence = originalDocument.sentences(sentenceIndex)
    val unveiledRaw = originalSentence.raw
    val unveiledStartOffsets = originalSentence.startOffsets
    val unveiledEndOffsets = originalSentence.endOffsets
    val unveiledWords = originalSentence.words
    val unveiledSentence = veiledSentence.copy(unveiledRaw, unveiledStartOffsets, unveiledEndOffsets, unveiledWords)

    def unveilStringArray(veiledArrayOpt: Option[Array[String]], veil: String): Option[Array[String]] =
        this.unveilStringArray(veiledArrayOpt, sentenceIndex, veil)

    unveiledSentence.tags     = unveilStringArray(unveiledSentence.tags,     Veil.veiledTag)
    unveiledSentence.lemmas   = unveilStringArray(unveiledSentence.lemmas,   Veil.veiledLemma)
    unveiledSentence.entities = unveilStringArray(unveiledSentence.entities, Veil.veiledEntity)
    unveiledSentence.norms    = unveilStringArray(unveiledSentence.norms,    Veil.veiledNorm)
    unveiledSentence.chunks   = unveilStringArray(unveiledSentence.chunks,   Veil.veiledChunk)

    unveiledSentence.syntacticTree = unveilSyntacticTree(unveiledSentence.syntacticTree)
    unveiledSentence.graphs = unveilGraphs(unveiledSentence.graphs, sentenceIndex)
    unveiledSentence.relations = unveilRelations(unveiledSentence.relations)
    unveiledSentence
  }

  protected def unveilDocument(veiledDocument: Document): Document = {
    val unveiledSentences = veiledDocument.sentences.zipWithIndex.map { case (veiledSentence, sentenceIndex) =>
      unveilSentence(veiledSentence, sentenceIndex)
    }
    val unveiledAnnotatedDocument = veiledDocument.copy(unveiledSentences)

    unveiledAnnotatedDocument
  }

  def annotate(processor: Processor): Document = {
    val veiledAnnotatedDocument = processor.annotate(veiledDocument)
    val unveiledAnnotatedDocument = unveilDocument(veiledAnnotatedDocument)

    unveiledAnnotatedDocument
  }
}

object VeilApp extends App {

  def veilText(processsor: Processor): Unit = {
    // Treat this text as if the letters "(Hahn-Powell, 2012)" did not exist
    // for the purpose of mkDocument, but do include them in the text.
    val text = "To be loved by unicorns is the greatest gift of all (Hahn-Powell, 2012)."
    val veiledLetters = Seq(Range.inclusive(text.indexOf('('), text.indexOf(')')))
    val veiledText = new VeiledText(text, veiledLetters)
    val document = veiledText.mkDocument(processor)

    new PrintWriter("veiledLetters.out").autoClose { printWriter =>
      val documentSerializer = new DocumentSerializer()

      documentSerializer.save(document, printWriter)
    }
  }

  def veilDocument(processor: Processor): Unit = {
    // Treat this text as if the words "( Hahn-Powell , 2012 )" did not exist
    // for the purpose of annotate, but do include them in the document.
    val text = "To be loved by unicorns is the greatest gift of all (Hahn-Powell, 2012)."
    val document = processor.mkDocument(text)
    val veiledWords = Seq((0, Range.inclusive(document.sentences(0).raw.indexOf("("), document.sentences(0).raw.indexOf(")"))))
    val veiledDocument = new VeiledDocument(document, veiledWords)
    val annotatedDocument = veiledDocument.annotate(processor)

    new PrintWriter("veiledWords.out").autoClose { printWriter =>
      val documentSerializer = new DocumentSerializer()

      documentSerializer.save(annotatedDocument, printWriter)
    }
  }

  val processor = new CluProcessor()

  veilText(processor)
  veilDocument(processor)
}
