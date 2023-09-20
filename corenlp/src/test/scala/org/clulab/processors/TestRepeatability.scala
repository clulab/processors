package org.clulab.processors

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.utils.{FileUtils, Sourcer, StringUtils, Test}

import java.io.File
import scala.util.Using

class TestRepeatability extends Test {

  def printDocument(document: Document): String = {
    StringUtils.viaPrintWriter { printWriter =>
      document.prettyPrint(printWriter)
    }
  }

  val processor: Processor = new FastNLPProcessor()

  behavior of "processing a document twice"

  it should "produce the same answer each time" in {
    val inputDir = FileUtils.getSubprojectDir("./corenlp/src/test/resources/documents")
    val file = new File(inputDir + "/16_South Sudan - Key Message Update_ Thu, 2018-01-25.txt")
    val text = {
      val text = Using.resource(Sourcer.sourceFromFile(file)) { source =>
        source.mkString.replace("\r\n", "\n")
      }

      val beginIndex = text.indexOf("This\nanalysis")
      val endIndex = text.indexOf("*According to the IPC")
//      val endIndex = text.indexOf("Region Contact")

      text.substring(beginIndex, endIndex).trim
    }

    val doc1 = processor.annotate(text)
    val result1 = printDocument(doc1)
    val result2 = printDocument(processor.annotate(text))
    val result3 = printDocument(doc1.copy())

    result1 should be (result2)
    result1 should be (result3)
  }
}
