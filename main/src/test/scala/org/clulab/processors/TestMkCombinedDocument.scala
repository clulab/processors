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
    Array(0, 5, 0, 5)
  )
  val separator = "  "
  val documentSerializer = new DocumentSerializer()
  val processor = new CluProcessor()

  def toString(document: Document): String = {
    val stringWriter = new StringWriter()

    new PrintWriter(stringWriter).autoClose { printWriter =>
      documentSerializer.save(document, printWriter, keepText = true)
    }
    stringWriter.toString
  }

  behavior of "mkCombinedDocument"

  def test(sentenceLengths: Array[Int], expectedResult: String): Unit = {
    val label = sentenceLengths.mkString("[", ", ", "]")

    it should s"combine $label" in {
      val sentenceStarts = sentenceLengths.scanLeft(0) { case (start, split) => start + split }
      assert(sentenceStarts.last == 10)
      val sentenceGroups = sentenceStarts.zip(sentenceLengths).map { case (start, length) =>
        sentences.slice(start, start + length).mkString(separator)
      }
      //
      val trailers = sentenceGroups.zipWithIndex.map { case (sentenceGroup, index) =>
        if (sentenceGroup.isEmpty || index == sentenceGroups.indices.last) ""
        else separator
      }
      val document = processor.mkCombinedDocument(sentenceGroups, trailers, keepText = true)
      val actualResult = toString(document)

      actualResult should be(expectedResult)
    }
  }

  {
    val document = processor.mkDocument(sentences.mkString(separator), keepText = true)
    val expectedResult = toString(document)

    manySentenceLengths.foreach { sentenceLengths =>
      test(sentenceLengths, expectedResult)
    }
  }

  behavior of "dynamically separated texts"

  it should "combine as expected" in {
    val text = "I found this text<br>on a web page."
    val separator = "<br>"
    val texts = text.split(separator)
    val indices = texts.indices
    val trailers = indices.map { index => if (index != indices.last) separator else "" }
    val document = processor.mkCombinedDocument(texts, trailers, keepText = true)

    document.text.get should be (text)
    document.sentences.length should be (2)

    document.sentences.foreach { sentence =>
      sentence.words should not contain(separator)
    }
  }
}
