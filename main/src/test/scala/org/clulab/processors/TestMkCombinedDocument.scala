package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.clulab.scala.Using._
import org.clulab.scala.WrappedArray._
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.{Sourcer, StringUtils, Test}

class TestMkCombinedDocument extends Test {
  val sentences = Using.resource(Sourcer.sourceFromFilename("./main/src/test/resources/org/clulab/processors/sentences10.txt")) { source =>
    source.getLines().toArray
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
    StringUtils.viaPrintWriter { printWriter =>
      documentSerializer.save(document, printWriter, keepText = true)
    }
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

  it should "include separators in both text and words" in {
    val text = "I found this text<br>on a web page."
    val separator = "<br>"
    val texts = text.split(separator)
    val dirtyTexts = texts.zipWithIndex.map { case (text, index) =>
      if (index != texts.indices.last) text + separator
      else text
    }
    val indices = texts.indices
    val trailers = indices.map { _ => "" }
    val document = processor.mkCombinedDocument(dirtyTexts, trailers, keepText = true)

    document.text.get should be (text)
    document.sentences.length should be (indices.length)

    document.sentences.zipWithIndex.foreach { case (sentence, index) =>
      if (index != indices.last)
        sentence.words should contain (separator)
      else
        sentence.words should not contain (separator)
    }
  }

  // This is thought to be the standard case.
  it should "include separators in text but not words" in {
    val text = "I found this text<br>on a web page."
    val separator = "<br>"
    val texts = text.split(separator)
    val indices = texts.indices
    val trailers = indices.map { index => if (index != indices.last) separator else "" }
    val document = processor.mkCombinedDocument(texts, trailers, keepText = true)

    document.text.get should be (text)
    document.sentences.length should be (indices.length)

    document.sentences.foreach { sentence =>
      sentence.words should not contain(separator)
    }
  }

  it should "include separators in neither text nor words" in {
    val text = "I found this text<br>on a web page."
    val separator = "<br>"
    val cleanSeparator = "    "
    val cleanText = text.replace(separator, cleanSeparator)
    val texts = text.split(separator)
    val indices = texts.indices
    val trailers = indices.map { index => if (index != indices.last) cleanSeparator else "" }
    val document = processor.mkCombinedDocument(texts, trailers, keepText = true)

    document.text.get should be(cleanText)
    document.sentences.length should be(indices.length)

    document.sentences.foreach { sentence =>
      sentence.words should not contain (separator)
    }
  }
}
