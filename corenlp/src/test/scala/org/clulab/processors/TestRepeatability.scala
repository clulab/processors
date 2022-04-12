package org.clulab.processors

import org.clulab.dynet.Utils
import org.clulab.processors.examples.ParallelProcessorExample
import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.FileUtils
import org.clulab.utils.Sourcer.utf8
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File
import java.io.PrintWriter
import java.io.StringWriter
import scala.collection.mutable
import scala.io.Source

class TestRepeatability extends FlatSpec with Matchers {
  Utils.initializeDyNet()

  def printDocument(document: Document): String = {
    val stringWriter = new StringWriter
    val printWriter = new PrintWriter(stringWriter)

    document.prettyPrint(printWriter)
    printWriter.close()
    stringWriter.toString
  }

  val processor: Processor = new FastNLPProcessorWithSemanticRoles()

  behavior of "processing a document twice"

  it should "produce the same answer each time" in {
    val inputDir = FileUtils.getSubprojectDir("./corenlp/src/test/resources/documents")
    val file = new File(inputDir + "/16_South Sudan - Key Message Update_ Thu, 2018-01-25.txt")
    val text = {
      val source = Source.fromFile(file, utf8)
      val text = source.mkString.replaceAllLiterally("\r\n", "\n")

      source.close()

      val beginIndex = text.indexOf("This\nanalysis")
      val endIndex = text.indexOf("*According to the IPC")
//      val endIndex = text.indexOf("Region Contact")

      text.substring(beginIndex, endIndex).trim
    }
    val result1 = printDocument(processor.annotate(text))
    val result2 = printDocument(processor.annotate(text))

    result1 should be (result2)
  }
}
