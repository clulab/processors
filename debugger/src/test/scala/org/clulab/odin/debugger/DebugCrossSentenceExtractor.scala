package org.clulab.odin.debugger

import org.clulab.odin.ExtractorEngine
import org.clulab.odin.debugger.odin.DebuggingExtractorEngine
import org.clulab.odin.impl.OdinConfig
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.utils.{FileUtils, Test}

import java.io.File

class DebugCrossSentenceExtractor extends Test {
  OdinConfig.keepRule = true

  val baseResourceDirName = "src/test/resources"
  val baseResourceName = "org/clulab/odin/debugger/CrossSentenceExtractor"
  val resourceDirName = if (!new File(baseResourceDirName).exists()) s"./debugger/$baseResourceDirName" else baseResourceDirName
  val resourceDir: File = new File(resourceDirName)

  val customLexiconNer = LexiconNER(Seq(s"$baseResourceName/FOOD.tsv"), Seq(true), Some(resourceDir))
  val processor = new CluProcessor(optionalNER = Some(customLexiconNer))
  val document = processor.annotate("John eats cake.  He does so often.")
  val sentence = document.sentences.last
  val ruleName = "coref"

  val badRules = FileUtils.getTextFromFile(new File(resourceDir, s"$baseResourceName/badMain.yml"))
  val badExtractorEngine = ExtractorEngine(badRules, ruleDir = Some(resourceDir))
  val badDebuggingExtractorEngine = DebuggingExtractorEngine(badExtractorEngine, active = true, verbose = false)
  val badDebuggingExtractor = badDebuggingExtractorEngine // .getExtractorByName(ruleName)
  val badMentions = badDebuggingExtractorEngine.extractFrom(document)

  val goodRules = FileUtils.getTextFromFile(new File(resourceDir, s"$baseResourceName/goodMain.yml"))
  val goodExtractorEngine = ExtractorEngine(goodRules, ruleDir = Some(resourceDir))
  val goodDebuggingExtractorEngine = DebuggingExtractorEngine(goodExtractorEngine, active = true, verbose = false)
  val goodDebuggingExtractor = goodDebuggingExtractorEngine // .getExtractorByName(ruleName)
  val goodMentions = goodDebuggingExtractorEngine.extractFrom(document)

  behavior of "debugger"

  it should "find problems with a CrossSentenceExtractor" in {
    Inspector(badDebuggingExtractorEngine)
        //.inspectSentence(sentence)
        //.inspectExtractor(badDebuggingExtractor)
        .inspectDynamicAsHtml("bad-crossSentenceExtractor-debug.html")
    Inspector(goodDebuggingExtractorEngine)
        //.inspectSentence(sentence)
        //.inspectExtractor(goodDebuggingExtractor)
        .inspectDynamicAsHtml("good-crossSentenceExtractor-debug.html")

    badMentions.length should be (2)
    goodMentions.length should be (3)
  }
}
