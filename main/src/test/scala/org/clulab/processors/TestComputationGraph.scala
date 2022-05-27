package org.clulab.processors

import org.clulab.dynet.Utils
import org.clulab.processors.clu.{CluProcessor, GivenConstEmbeddingsAttachment}
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.{FileUtils, Test}

class TestComputationGraph extends Test {
  // For multiple tests, pass in the document name so all found in directory.
  val expectedOutputDir = "./main/src/test/resources/TestComputationGraph"

  class TestCluProcessor extends CluProcessor {
    type Stage = Document => Unit
    val documentSerializer = new DocumentSerializer()

    case class NamedStage(name: String, stage: Stage) {
      val expectedOutput = FileUtils.getTextFromFile(s"$expectedOutputDir/$name.txt").replaceAll("\r\n", "\n")

      def execute(document: Document): Unit = {
        println(s"Starting $name...")
        stage(document)

        val actualOutput = documentSerializer
            .save(document)
            .replaceAll("\r\n", "\n")

        if (expectedOutput != actualOutput)
          println("It failed!")
        println(s"Ending $name.")
      }
    }

    val namedStages = Array(
      NamedStage("tagPartsOfSpeech", tagPartsOfSpeech),
      NamedStage("recognizeNamedEntities", recognizeNamedEntities),
      NamedStage("chunking", chunking),
      NamedStage("parse", parse),
      NamedStage("lemmatize", lemmatize),
      NamedStage("srl", srl),
      NamedStage("resolveCoreference", resolveCoreference),
      NamedStage("discourse", discourse)
    )

    override def annotate(doc: Document): Document = {
      GivenConstEmbeddingsAttachment(doc).perform {
        namedStages.foreach(_.execute(doc))

        doc.clear()
        doc
      }
    }
  }

  Utils.initializeDyNet()

  val text = "Livestock market prices are likely to decline further from now as their body condition is getting worse until the next rainy season starts. If the start of the next rainy season is late, further deterioration and death of livestock will occur so that demand in the market will sharply decline. On the other hand, the staple food price through September 2017 will to rise seasonally and remain somewhat above average."
  val processor = new TestCluProcessor()

  // Make sure it can be done once.
  processor.annotate(text)
  // Make sure it is consistent.
  processor.annotate(text)

  // Make sure it can be done in parallel.
  1.to(4).par.foreach { index =>
    println(s"Starting thread $index...")
    processor.annotate(text)
    println(s"Ending thread $index...")
  }
}
