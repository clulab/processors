package org.clulab.odin.debugger.extractor

import org.clulab.odin.ExtractorEngine
import org.clulab.odin.debugger.debug.filter.DynamicDebuggerFilter
import org.clulab.odin.debugger.odin.DebuggingExtractorEngine
import org.clulab.odin.debugger.{DebugTest, Inspector}
import org.clulab.odin.impl.{CrossSentenceExtractor, OdinConfig}
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.FileUtils

import java.io.File

class DebugCrossSentenceExtractor extends DebugTest {
  OdinConfig.keepRule = true

  val baseResourceDirName = "src/test/resources"
  val baseResourceName = "org/clulab/odin/debugger/CrossSentenceExtractor"
  val resourceDirName = if (!new File(baseResourceDirName).exists()) s"./debugger/$baseResourceDirName" else baseResourceDirName
  val resourceDir: File = new File(resourceDirName)

  val processor = new CluProcessor()
  val document = processor.annotate("John eats cake.  He does so often.", keepText = true)
  val sentences = Seq(document.sentences.head, document.sentences.last)
  val ruleName = "coref"

  val badRules = FileUtils.getTextFromFile(new File(resourceDir, s"$baseResourceName/badMain.yml"))
  val badExtractorEngine = ExtractorEngine(badRules, ruleDir = Some(resourceDir))
  val badDebuggingExtractorEngine = DebuggingExtractorEngine(badExtractorEngine, active = true, verbose = false)
  val badMentions = badDebuggingExtractorEngine.extractFrom(document)
  val badDebuggingExtractor = badDebuggingExtractorEngine.getExtractorByName(ruleName).asInstanceOf[CrossSentenceExtractor]
  val badDynamicDebuggerFilter = DynamicDebuggerFilter.crossSentenceExtractorFilter(badDebuggingExtractor).sentencesFilter(sentences)

  val goodRules = FileUtils.getTextFromFile(new File(resourceDir, s"$baseResourceName/goodMain.yml"))
  val goodExtractorEngine = ExtractorEngine(goodRules, ruleDir = Some(resourceDir))
  val goodDebuggingExtractorEngine = DebuggingExtractorEngine(goodExtractorEngine, active = true, verbose = false)
  val goodMentions = goodDebuggingExtractorEngine.extractFrom(document)
  val goodDebuggingExtractor = goodDebuggingExtractorEngine.getExtractorByName(ruleName).asInstanceOf[CrossSentenceExtractor]
  val goodDynamicDebuggerFilter = DynamicDebuggerFilter.crossSentenceExtractorFilter(goodDebuggingExtractor).sentencesFilter(sentences)

  behavior of "debugger"

  it should "find problems with a CrossSentenceExtractor" in {
    Inspector(badDebuggingExtractorEngine)
        .filter(badDynamicDebuggerFilter)
        .inspectDynamicAsHtml("../debug-dynamic-crossSentenceExtractor-bad.html")
    Inspector(goodDebuggingExtractorEngine)
        .filter(goodDynamicDebuggerFilter)
        .inspectDynamicAsHtml("../debug-dynamic-crossSentenceExtractor-good.html")

    badMentions.length should be (2)
    goodMentions.length should be (3)
  }
}
