package org.clulab.processors

import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.utils.{FileUtils, StringUtils, Test}
import org.clulab.utils.Sourcer.utf8

import java.io.File
import scala.io.Source
import scala.util.Using

class TestRepeatability extends Test {

  def printDocument(document: Document): String = {
    StringUtils.viaPrintWriter { printWriter =>
      document.prettyPrint(printWriter)
    }
  }

  val processor: Processor = new FastNLPProcessorWithSemanticRoles()

  behavior of "processing a document twice"

  it should "produce the same answer each time" in {
    val inputDir = FileUtils.getSubprojectDir("./corenlp/src/test/resources/documents")
    val file = new File(inputDir + "/16_South Sudan - Key Message Update_ Thu, 2018-01-25.txt")
    val text = {
      val text = Using.resource(Source.fromFile(file, utf8)) { source =>
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
