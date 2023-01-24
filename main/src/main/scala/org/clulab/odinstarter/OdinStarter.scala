package org.clulab.odinstarter

import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import java.io.File
import org.clulab.odin.ExtractorEngine
import org.clulab.utils.FileUtils
import org.clulab.odin.Mention



object OdinStarter extends App {
  val resourceDir: File = {
    val cwd = new File(System.getProperty("user.dir"))
    new File(cwd, "src/main/resources")
  }

  /** Custom NER for our grammar */
  def newLexiconNer(): LexiconNER = {
    // note: if adding a new lexicon, add another Bool value in the sequence that is an argument to LexiconNER a few lines down in this method
    val kbs = Seq(
      "org/clulab/odinstarter/FOOD.tsv"
    )
    val isLocal = kbs.forall(new File(resourceDir, _).exists)
    val lexiconNer = LexiconNER(kbs,
      Seq(
        true // case insensitive match 
      ),
      if (isLocal) Some(resourceDir) else None
    )

    lexiconNer
  }

  /** Creates an Odin extractor engine */
  def newExtractorEngine(masterResource: String = "/org/clulab/odinstarter/master.yml"): ExtractorEngine = {
    // We usually want to reload rules during development,
    // so we try to load them from the filesystem first, then jar.
    val masterFile = new File(resourceDir, masterResource.drop(1)) // the resource path must start with /
    if (masterFile.exists()) {
      // read file from filesystem
      val rules = FileUtils.getTextFromFile(masterFile)
      ExtractorEngine(rules, ruleDir = Some(resourceDir))
    }
    else {
      // read rules from yml file in resources
      val rules = FileUtils.getTextFromResource(masterResource)
      // creates an extractor engine using the rules and the default actions
      ExtractorEngine(rules)
    }
  }

  def printMention(m: Mention): Unit = {
    println("MENTION:")
    println("Labels: " + m.labels.mkString(" "))
    println("Sentence: " + m.sentenceObj.words.mkString(" "))
    println("Tokens: " + m.tokenInterval)
    if(m.arguments.nonEmpty) {
      println("Arguments:")
      for(name <- m.arguments.keys) {
        println("Argument name: " + name)
        for(arg <- m.arguments(name)) {
          println("Argument value:")
          printMention(arg)
        }
      }
    }
  }

  val proc = new CluProcessor(optionalNER = Some(newLexiconNer()))
  val engine = newExtractorEngine()

  val doc = proc.annotate("John eats cake.")
  val mentions = engine.extractFrom(doc)
  for(mention <- mentions) {
    printMention(mention)
  }
}
