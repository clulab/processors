package org.clulab.odin.debugger.extractor.graph

import org.clulab.odin.ExtractorEngine
import org.clulab.odin.debugger.debug.filter.DynamicDebuggerFilter
import org.clulab.odin.debugger.odin.DebuggingExtractorEngine
import org.clulab.odin.debugger.{DebugTest, Inspector}
import org.clulab.odin.impl.{GraphExtractor, OdinConfig}
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.utils.FileUtils

import java.io.File

class DebugRelationGraphExtractor extends DebugTest {
  OdinConfig.keepRule = true

  val baseResourceDirName = "src/test/resources"
  val baseResourceName = "org/clulab/odin/debugger/GraphExtractor/relation"
  val resourceDirName = if (!new File(baseResourceDirName).exists()) s"./debugger/$baseResourceDirName" else baseResourceDirName
  val resourceDir: File = new File(resourceDirName)

  val customLexiconNer = LexiconNER(Seq(s"$baseResourceName/FOOD.tsv"), Seq(true), Some(resourceDir))
  val processor = new CluProcessor(optionalNER = Some(customLexiconNer))
  val document = processor.annotate("John eats cake.", keepText = true)
  val sentence = document.sentences.head
  val ruleName = "people-eat-food"

  val badRules = FileUtils.getTextFromFile(new File(resourceDir, s"$baseResourceName/badMain.yml"))
  val badExtractorEngine = ExtractorEngine(badRules, ruleDir = Some(resourceDir))
  val badDebuggingExtractorEngine = DebuggingExtractorEngine(badExtractorEngine, active = true, verbose = false)
  val badMentions = badDebuggingExtractorEngine.extractFrom(document)
  val badDebuggingExtractor = badDebuggingExtractorEngine.getExtractorByName(ruleName).asInstanceOf[GraphExtractor]
  val badDynamicDebuggerFilter = DynamicDebuggerFilter.graphExtractorFilter(badDebuggingExtractor).sentenceFilter(sentence)

  val goodRules = FileUtils.getTextFromFile(new File(resourceDir, s"$baseResourceName/goodMain.yml"))
  val goodExtractorEngine = ExtractorEngine(goodRules, ruleDir = Some(resourceDir))
  val goodDebuggingExtractorEngine = DebuggingExtractorEngine(goodExtractorEngine, active = true, verbose = false)
  val goodMentions = goodDebuggingExtractorEngine.extractFrom(document)
  val goodDebuggingExtractor = goodDebuggingExtractorEngine.getExtractorByName(ruleName).asInstanceOf[GraphExtractor]
  val goodDynamicDebuggerFilter = DynamicDebuggerFilter.graphExtractorFilter(goodDebuggingExtractor).sentenceFilter(sentence)

  behavior of "debugger"

  it should "find problems with a GraphExtractor" in {
    Inspector(badDebuggingExtractorEngine)
        .filter(badDynamicDebuggerFilter)
        .inspectDynamicAsHtml("../debug-dynamic-relationGraphExtractor-bad.html")
    Inspector(goodDebuggingExtractorEngine)
        .filter(goodDynamicDebuggerFilter)
        .inspectDynamicAsHtml("../debug-dynamic-relationGraphExtractor-good.html")

    badMentions.length should be (3)
    goodMentions.length should be (4)
  }
}
