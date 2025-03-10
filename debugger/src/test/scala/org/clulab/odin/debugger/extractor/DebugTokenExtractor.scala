package org.clulab.odin.debugger.extractor

import org.clulab.odin.ExtractorEngine
import org.clulab.odin.debugger.debug.filter.DynamicDebuggerFilter
import org.clulab.odin.debugger.odin.DebuggingExtractorEngine
import org.clulab.odin.debugger.{DebugTest, Inspector}
import org.clulab.odin.impl.{OdinConfig, TokenExtractor}
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.FileUtils

import java.io.File

class DebugTokenExtractor extends DebugTest {
  OdinConfig.keepRule = true

  val baseResourceDirName = "src/test/resources"
  val baseResourceName = "org/clulab/odin/debugger/TokenExtractor"
  val resourceDirName = if (!new File(baseResourceDirName).exists()) s"./debugger/$baseResourceDirName" else baseResourceDirName
  val resourceDir: File = new File(resourceDirName)
  val processor = new CluProcessor()
  val document = processor.annotate("John eats cake.", keepText = true)
  val sentence = document.sentences.head
  val ruleName = "person"

  val badRules = FileUtils.getTextFromFile(new File(resourceDir, s"$baseResourceName/badMain.yml"))
  val badExtractorEngine = ExtractorEngine(badRules, ruleDir = Some(resourceDir))
  val badDebuggingExtractorEngine = DebuggingExtractorEngine(badExtractorEngine, active = true, verbose = false)
  val badMentions = badDebuggingExtractorEngine.extractFrom(document)
  val badDebuggingExtractor = badDebuggingExtractorEngine.getExtractorByName(ruleName).asInstanceOf[TokenExtractor]
  val badDynamicDebuggerFilter = DynamicDebuggerFilter.tokenExtractorFilter(badDebuggingExtractor).sentenceFilter(sentence)

  val goodRules = FileUtils.getTextFromFile(new File(resourceDir, s"$baseResourceName/goodMain.yml"))
  val goodExtractorEngine = ExtractorEngine(goodRules, ruleDir = Some(resourceDir))
  val goodDebuggingExtractorEngine = DebuggingExtractorEngine(goodExtractorEngine, active = true, verbose = false)
  val goodMentions = goodDebuggingExtractorEngine.extractFrom(document)
  val goodDebuggingExtractor = goodDebuggingExtractorEngine.getExtractorByName(ruleName).asInstanceOf[TokenExtractor]
  val goodDynamicDebuggerFilter = DynamicDebuggerFilter.tokenExtractorFilter(goodDebuggingExtractor).sentenceFilter(sentence)

  behavior of "debugger"

  it should "find problems with a TokenExtractor" in {
    Inspector(badDebuggingExtractorEngine)
        .filter(badDynamicDebuggerFilter)
        .inspectStaticAsHtml("../debug-static-tokenExtractor-bad.html")
        .inspectDynamicAsHtml("../debug-dynamic-tokenExtractor-bad.html")
    Inspector(goodDebuggingExtractorEngine)
        .filter(goodDynamicDebuggerFilter)
        .inspectStaticAsHtml("../debug-static-tokenExtractor-good.html")
        .inspectDynamicAsHtml("../debug-dynamic-tokenExtractor-good.html")

    badMentions.length should be (0)
    goodMentions.length should be (1)
  }
}
