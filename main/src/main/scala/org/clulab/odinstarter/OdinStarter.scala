package org.clulab.odinstarter

import org.clulab.odin.ExtractorEngine
import org.clulab.odin.Mention
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.utils.FileUtils

import java.io.File

object OdinStarter extends App {
  // When using an IDE rather than sbt, make sure the working directory for the run
  // configuration is the subproject directory so that this location is accessible.
  val resourceDir: File = new File("./src/main/resources")
  val customLexiconNer = { // i.e., Named Entity Recognizer
    val kbsAndCaseInsensitiviteMatchings: Seq[(String, Boolean)] = Seq(
      // You can add additional kbs (knowledge bases) and caseInsensitiveMatchings here.
      ("org/clulab/odinstarter/FOOD.tsv", true) // ,
      // ("org/clulab/odinstarter/RESTAURANTS.tsv", false)
    )
    val kbs = kbsAndCaseInsensitiviteMatchings.map(_._1)
    val caseInsensitiveMatchings = kbsAndCaseInsensitiviteMatchings.map(_._2)
    val isLocal = kbs.forall(new File(resourceDir, _).exists)
    val baseDirOpt = if (isLocal) Some(resourceDir) else None

    LexiconNER(kbs, caseInsensitiveMatchings, baseDirOpt)
  }
  val processor = new CluProcessor(optionalNER = Some(customLexiconNer))
  val extractorEngine = {
    val masterResource = "/org/clulab/odinstarter/main.yml"
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
  val document = processor.annotate("John eats cake.")
  val mentions = extractorEngine.extractFrom(document).sortBy(_.arguments.size)

  for (mention <- mentions)
    printMention(mention)

  def printMention(mention: Mention, depth: Int = 0): Unit = {
    val tab = "    "
    val indent = tab * depth

    println(indent + "  MENTION:")
    println(indent + "   Labels: " + mention.labels.mkString(" "))
    println(indent + " Sentence: " + mention.sentenceObj.words.mkString(" "))
    println(indent + "   Tokens: " + mention.tokenInterval.map(mention.sentenceObj.words).mkString(" "))
    if (mention.arguments.nonEmpty) {
      println(indent + "Arguments:")
      for ((name, mentions) <- mention.arguments) {
        println(indent + tab + "     Name: " + name)
        for (mention <- mentions)
          printMention(mention, depth + 1)
      }
    }
    println()
  }
}
