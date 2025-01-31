package org.clulab.odin.debugger.apps

import org.clulab.odin.debugger.debugging.DebuggingExtractorEngine
import org.clulab.odin.debugger.inspector.Inspector
import org.clulab.odin.debugger.visualizer.TextVisualizer
import org.clulab.odin.{ExtractorEngine, Mention}
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.utils.FileUtils

import java.io.File

object DebuggerStarterApp extends App {
  // When using an IDE rather than sbt, make sure the working directory for the run
  // configuration is the subproject directory so that this resourceDir is accessible.
  val resourceDir: File = new File("./src/main/resources")
  val customLexiconNer = { // i.e., Named Entity Recognizer
    val kbsAndCaseInsensitiveMatchings: Seq[(String, Boolean)] = Seq(
      // You can add additional kbs (knowledge bases) and caseInsensitiveMatchings here.
      ("org/clulab/odin/debugger/FOOD.tsv", true) // ,
      // ("org/clulab/odinstarter/RESTAURANTS.tsv", false)
    )
    val kbs = kbsAndCaseInsensitiveMatchings.map(_._1)
    val caseInsensitiveMatchings = kbsAndCaseInsensitiveMatchings.map(_._2)
    val isLocal = kbs.forall(new File(resourceDir, _).exists)
    val baseDirOpt = if (isLocal) Some(resourceDir) else None

    LexiconNER(kbs, caseInsensitiveMatchings, baseDirOpt)
  }
  val processor = new CluProcessor(optionalNER = Some(customLexiconNer))
  val extractorEngine = {
    val masterResource = "/org/clulab/odin/debugger/main.yml"
    // We usually want to reload rules during development,
    // so we try to load them from the filesystem first, then jar.
    // The resource must start with /, but the file probably shouldn't.
    val masterFile = new File(resourceDir, masterResource.drop(1))

    if (masterFile.exists) {
      // Read rules from file in filesystem.
      val rules = FileUtils.getTextFromFile(masterFile)
      ExtractorEngine(rules, ruleDir = Some(resourceDir))
    }
    else {
      // Read rules from resource in jar.
      val rules = FileUtils.getTextFromResource(masterResource)
      ExtractorEngine(rules, ruleDir = None)
    }
  }
  val document = processor.annotate("B A A C A A A D A A A A A E")
  val mentions = extractorEngine.extractFrom(document).sortBy(_.arguments.size)

  for (mention <- mentions)
    printMention(mention)

  val debuggingExtractorEngine = DebuggingExtractorEngine(extractorEngine)
  val debuggingMentions = debuggingExtractorEngine.extractFrom(document).sortBy(_.arguments.size)
  val extractors = extractorEngine.extractors
  val visualizer = new TextVisualizer()
  val inspector = new Inspector(debuggingExtractorEngine.transcript)

  assert(mentions.length == debuggingMentions.length)
  extractors.foreach { extractor =>
    visualizer.visualize(extractor)
    println()
  }

  def printMention(mention: Mention, nameOpt: Option[String] = None, depth: Int = 0): Unit = {
    val indent = "    " * depth
    val name = nameOpt.getOrElse("<none>")
    val labels = mention.labels
    val words = mention.sentenceObj.words
    val tokens = mention.tokenInterval.map(mention.sentenceObj.words)

    println(indent + "     Name: " + name)
    println(indent + "   Labels: " + labels.mkString(" "))
    println(indent + " Sentence: " +  words.mkString(" "))
    println(indent + "   Tokens: " + tokens.mkString(" "))
    if (mention.arguments.nonEmpty) {
      println(indent + "Arguments:")
      for ((name, mentions) <- mention.arguments; mention <- mentions)
        printMention(mention, Some(name), depth + 1)
    }
    println()
  }
}
