package org.clulab.odin.debugger

import org.clulab.odin.ExtractorEngine
import org.clulab.odin.debugger.debugging.DebuggingExtractorEngine
import org.clulab.odin.debugger.visualizer.extractor.{HtmlExtractorVisualizer, TextExtractorVisualizer}
import org.clulab.odin.debugger.visualizer.rule.TextRuleVisualizer
import org.clulab.odin.impl.{Done, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, OdinConfig, Pass, SaveEnd, SaveStart, Split}
import org.clulab.utils.{FileUtils, Test}

import java.io.File

class DebugInst extends Test {
  OdinConfig.keepRule = true

  val baseResourceDirName = "src/test/resources"
  val resourceDirName = if (!new File(baseResourceDirName).exists()) s"./debugger/$baseResourceDirName" else baseResourceDirName
  val resourceDir: File = new File(resourceDirName)
  val rules = FileUtils.getTextFromFile(new File(resourceDir, "org/clulab/odin/debugger/Inst/main.yml"))
  val extractorEngine = ExtractorEngine(rules, ruleDir = Some(resourceDir))
  val debuggingExtractorEngine = DebuggingExtractorEngine(extractorEngine, active = true, verbose = false)
  val textRuleVisualizer = new TextRuleVisualizer()
  val textExtractorVisualizer = new TextExtractorVisualizer()
  val htmlExtractorVisualizer = new HtmlExtractorVisualizer()
  val instNames = Seq(
    Done,
    Pass,
    Split,
    SaveStart,
    SaveEnd,
    MatchToken,
    MatchMention,
    MatchSentenceStart,
    MatchSentenceEnd,
    MatchLookAhead,
    MatchLookBehind
  ).map { inst => inst.getClass.getSimpleName.dropRight(if (inst == Done) 0 else 1) }

  behavior of "debugger"

  it should "output an Inst of every known kind" in {
    val extractor = debuggingExtractorEngine.extractors.head
    val textRuleVisualization = textRuleVisualizer.visualize(extractor).toString
    val textExtractorVisualization = textExtractorVisualizer.visualize(extractor).toString
    val htmlExtractorVisualization = htmlExtractorVisualizer.visualize(extractor).toString

    println(textRuleVisualization)
    println(textExtractorVisualization)
    instNames.foreach { instName =>
      textExtractorVisualization should include (instName)
      htmlExtractorVisualization should include (instName)
    }
  }
}
