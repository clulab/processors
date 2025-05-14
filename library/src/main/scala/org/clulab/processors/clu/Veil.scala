package org.clulab.processors.clu

import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.struct.{DirectedGraph, Edge, GraphMap, RelationTriple, Tree}
import org.clulab.struct.GraphMap._

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
  * See VeilApp.veilText for an example.
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
    val unveiledDocument = veiledDocument.copy(text = Some(originalText))

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
  * See VeilApp.veilDocument for an example.
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
  /** There is one array per sentence and it contains at each index the index where a value (e.g., word in
    * an array of words) should be transferred as it is unveiled.  Code using the unveilArrays might look like
    * unveiledValues(unveilArrays(sentenceIndex)(veiledIndex)) = veiledValues(veiledIndex)
    */
  protected lazy val unveilArrays = {
    val arrays = originalDocument.sentences.zip(veilSets).map { case (originalSentence, set) =>
      val array = new Array[Int](originalSentence.words.length - set.size)
      var unveiledIndex = 0

      array.indices.foreach { veiledIndex =>
        while (set(unveiledIndex))
          unveiledIndex += 1
        array(veiledIndex) = unveiledIndex
        unveiledIndex += 1
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

    val tags     = unveilStringArray(unveiledSentence.tags,     Veil.veiledTag)
    val lemmas   = unveilStringArray(unveiledSentence.lemmas,   Veil.veiledLemma)
    val entities = unveilStringArray(unveiledSentence.entities, Veil.veiledEntity)
    val norms    = unveilStringArray(unveiledSentence.norms,    Veil.veiledNorm)
    val chunks   = unveilStringArray(unveiledSentence.chunks,   Veil.veiledChunk)

    val syntacticTree = unveilSyntacticTree(unveiledSentence.syntacticTree)
    val graphs = unveilGraphs(unveiledSentence.graphs, sentenceIndex)
    val relations = unveilRelations(unveiledSentence.relations)

    val newSentence = Sentence(
      unveiledSentence.raw, unveiledSentence.startOffsets, unveiledSentence.endOffsets, unveiledSentence.words,
      tags, lemmas, entities, norms, chunks, syntacticTree, graphs, relations
    )
    newSentence
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
