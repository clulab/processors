package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.{Sourcer, Test}

import java.io.{PrintWriter, StringWriter}

class TestMkCombinedDocument extends Test {
  val sentences = Sourcer.sourceFromFilename("./main/src/test/resources/org/clulab/processors/sentences10.txt").autoClose { source =>
    source.getLines.toArray
  }
  val manySentenceLengths = Array(
    Array(1, 9),
    Array(9, 1),
    Array(1, 1, 8),
    Array(1, 8, 1),
    Array(8, 1, 1),
    Array(5, 5),
    Array(2, 2, 2, 2, 2),
    Array(1, 2, 3, 4),
    Array(4, 3, 2, 1),
    Array(0, 5, 0, 5, 0)
  )
  val sep = "  "
  val documentSerializer = new DocumentSerializer()

  def toString(document: Document): String = {
    val stringWriter = new StringWriter()

    new PrintWriter(stringWriter).autoClose { printWriter =>
      documentSerializer.save(document, printWriter)
    }
    stringWriter.toString
  }

  behavior of "mkCombinedDocument"

  def test(sentenceLengths: Array[Int]): Unit = {
    val label = sentenceLengths.mkString("[", ", ", "]")

    it should s"combine $label" in {
      val sentenceStarts = sentenceLengths.scanLeft(0) { case (start, split) => start + split }
      assert(sentenceStarts.last == 10)
      val sentenceGroups = sentenceStarts.zip(sentenceLengths).map { case (start, length) =>
        sentences.slice(start, start + length).mkString(sep)
      }
      val separators = sentenceGroups.map { sentenceGroup => if (sentenceGroup.isEmpty) "" else sep }
      val document = processor.mkCombinedDocument(sentenceGroups, separators)
      val actualResult = toString(document)

      actualResult should be(expectedResult)
    }
  }

  val processor = new CluProcessor()
  val document = processor.mkDocument(sentences.mkString(sep))
  val expectedResult = toString(document)

  manySentenceLengths.foreach { sentenceLengths =>
    test(sentenceLengths)
  }


  // Do another example with <br> in the middle.  Make sure get two sentences.
}
