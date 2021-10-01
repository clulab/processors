package org.clulab.processors

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.serialization.json4StopGraph.JSONSerializer
import org.clulab.utils.{FileUtils, StringUtils}

import java.io.{File, PrintWriter}

object ProcessorJsonBatch {
  def main(args: Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)

    val inputDir = props.getProperty("in")
    assert(inputDir != null)
    val outputDir = props.getProperty("out")
    assert(outputDir != null)

    Utils.initializeDyNet()
    val proc = new CluProcessor()

    for (file <- FileUtils.findFiles(inputDir, ".txt")) {
      val fileName = file.getName
      val text = FileUtils.getTextFromFile(file)
      val doc = proc.annotate(text)

      val outputFile = outputDir + File.separator + fileName + ".json"
      val pw = new PrintWriter(outputFile)
      val serializer = new JSONSerializer(pw)
      pw.close()
    }
  }
}
