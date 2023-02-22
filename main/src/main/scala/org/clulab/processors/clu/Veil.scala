package org.clulab.processors.clu

import org.clulab.processors.{Document, Processor}
import org.clulab.serialization.DocumentSerializer
import org.clulab.struct.{DirectedGraph, Edge, GraphMap}
import org.clulab.utils.Closer.AutoCloser

import java.io.PrintWriter
import scala.collection.mutable.{Set => MutableSet}

trait Veil

class VeiledText(originalText: String, veiledLetters: Seq[Range]) extends Veil {
  protected lazy val veiledText: String = {
    val letters = new StringBuffer(originalText)
    val indices = originalText.indices

    veiledLetters.foreach { range =>
      range.foreach { index =>
        if (indices.contains(index))
          letters.setCharAt(index, ' ')
      }
    }
    letters.toString
  }

  protected def unveil(veiledDocument: Document): Document = {
    val unveiledDocument = veiledDocument.copy(textOpt = Some(originalText))

    unveiledDocument
  }

  def mkDocument(processor: Processor): Document = {
    val veiledDocument = processor.mkDocument(veiledText, keepText = false)
    val unveiledDocument = unveil(veiledDocument)

    unveiledDocument
  }
}

class VeiledDocument(originalDocument: Document, veiledWords: Seq[(Int, Range)]) extends Veil {
  // This is an array of sets, each containing veiled word indices for each sentence.
  protected lazy val veilSets = {
    val sets = Array.fill(originalDocument.sentences.length)(MutableSet.empty[Int])

    veiledWords.foreach { case (sentenceIndex, wordRange) =>
      if (sets.indices.contains(sentenceIndex)) {
        val set = sets(sentenceIndex)
        val wordIndexes = originalDocument.sentences(sentenceIndex).words.indices

        wordRange.foreach { wordIndex =>
          if (wordIndexes.contains(wordIndex))
            set += wordIndex
        }
      }
    }
    sets
  }
  // This is an array of arrays, each containing at an index the index of the unveiled value
  // that should be used in the result.  If the value is -1, then that index had been veiled.
  protected lazy val unveilArrays = {
    val arrays = originalDocument.sentences.zip(veilSets).map { case (originalSentence, set) =>
      val array = new Array[Int](originalSentence.words.length)
      var veiledIndex = 0
// TODO: These at the same time!
      array.indices.foreach { originalIndex =>
        if (set(originalIndex))
          array(originalIndex) = -1 // This word was deleted.
        else {
          array(originalIndex) = veiledIndex
          veiledIndex += 1
        }
      }
      array
    }

    arrays
  }
  protected lazy val ununveilArrays = {
    // What should this be called?
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
      val wordIndexes = originalSentence.words.indices.filter { wordIndex => !veilSets(sentenceIndex)(wordIndex) }.toArray
      val veiledRaw = wordIndexes.map(originalSentence.raw)
      val veiledStartOffsets = wordIndexes.map(originalSentence.startOffsets)
      val veiledEndOffsets = wordIndexes.map(originalSentence.endOffsets)
      val veiledWords = wordIndexes.map(originalSentence.words)
      val veiledSentence = originalSentence.copy(veiledRaw, veiledStartOffsets, veiledEndOffsets, veiledWords)

      veiledSentence
    }

    originalDocument.copy(veiledSentences)
  }

  protected def unveil(veiledDocument: Document): Document = {
    val unveiledSentences = veiledDocument.sentences.zipWithIndex.map { case (veiledSentence, sentenceIndex) =>
      val originalSentence = originalDocument.sentences(sentenceIndex)
      val unveiledRaw = originalSentence.raw
      val unveiledStartOffsets = originalSentence.startOffsets
      val unveiledEndOffsets = originalSentence.endOffsets
      val unveiledWords = originalSentence.words
      val unveiledSentence = veiledSentence.copy(unveiledRaw, unveiledStartOffsets, unveiledEndOffsets, unveiledWords)
      val unveilArray = unveilArrays(sentenceIndex)
      val ununveilArray = ununveilArrays(sentenceIndex)

      def unveilStrings(veiledArray: Array[String]): Array[String] = {
        val unveiledArray = Array.tabulate(unveilArray.length) { unveiledIndex =>
          val veiledIndex = unveilArray(unveiledIndex)
          // Put at the unveiled index what was at the veiled index.
          if (veiledIndex != -1) veiledArray(veiledIndex) else "?"
        }

        unveiledArray
      }

      def unveilGraphs(veiledGraphs: GraphMap): GraphMap = {
        val unveiledGraphs = GraphMap()

        veiledGraphs.foreach { case (name, directedGraph) =>
          val veiledEdges = directedGraph.allEdges.map { case (veiledSource, veiledDestination, relation) =>
            val unveiledSource = ununveilArray(veiledSource)
            val unveiledDestination = ununveilArray(veiledDestination)

            Edge(unveiledSource, unveiledDestination, relation)
          }
          val veiledSize = unveilArray.size
          val veiledRoots = directedGraph.roots.map { root =>
            ununveilArray(root)
          }
// TODO still need to shove left or right
          unveiledGraphs(name) = new DirectedGraph(veiledEdges, Some(veiledSize), Some(veiledRoots))
        }
        unveiledGraphs
      }

      unveiledSentence.tags = unveiledSentence.tags.map(unveilStrings)
      unveiledSentence.lemmas = unveiledSentence.lemmas.map(unveilStrings)
      unveiledSentence.entities = unveiledSentence.entities.map(unveilStrings)
      unveiledSentence.norms = unveiledSentence.norms.map(unveilStrings)
      unveiledSentence.chunks = unveiledSentence.chunks.map(unveilStrings)
//      unveiledSentence.syntacticTree
      unveiledSentence.graphs = unveilGraphs(unveiledSentence.graphs)
//      unveiledSentence.relations
      unveiledSentence
    }
    val unveiledAnnotatedDocument = veiledDocument.copy(unveiledSentences)

    unveiledAnnotatedDocument
  }

  def annotate(processor: Processor): Document = {
    val veiledAnnotatedDocument = processor.annotate(veiledDocument)
    val unveiledAnnotatedDocument = unveil(veiledAnnotatedDocument)

    unveiledAnnotatedDocument
  }
}

object VeilApp extends App {
  val processor = new CluProcessor()

  if (false)
  {
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

  if (true)
  {
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
}
