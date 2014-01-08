package edu.arizona.sista.processor.fastnlp

import edu.arizona.sista.processor.corenlp.CoreNLPProcessor
import edu.arizona.sista.processor.{Sentence, Document}
import org.maltparserx.MaltParserService
import FastNLPProcessor._
import scala.collection.mutable.ArrayBuffer
import java.io.{IOException, File}

/**
 * Fast NLP tools
 * Uses most of CoreNLP but replaces its parser with maltparser
 * This means that constituent trees and coreference, which depends on that, are not available
 * User: mihais
 * Date: 1/4/14
 */
class FastNLPProcessor(internStrings:Boolean = true) extends CoreNLPProcessor(internStrings) {
  private lazy val maltService = new MaltParserService()
  private var serviceInitialized = false

  override def parse(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return
    if (doc.sentences.head.tags == None)
      throw new RuntimeException("ERROR: you have to run the POS tagger before NER!")
    if (doc.sentences.head.lemmas == None)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before NER!")

    // load models if not done already
    initializeService()

    // parse each individual sentence
    for(sentence <- doc.sentences) {
      parseSentence(sentence)
    }
  }

  /** Parses one sentence and stores the dependency graph in the sentence object */
  private def parseSentence(sentence:Sentence) {
    // tokens stores the tokens in the input format expected by malt (CoNLL-X)
    val tokens = new Array[String](sentence.words.length)
    for(i <- 0 until tokens.length) {
      tokens(i) = s"${i + 1}\t${sentence.words(i)}\t${sentence.lemmas.get(i)}\t${sentence.tags.get(i)}\t${sentence.tags.get(i)}\t_"
    }

    // the actual parsing
    val output = maltService.parseTokens(tokens)

    // convert malt's output into our dependency graph
    for(o <- output) println(o)
    // TODO
  }

  private def initializeService() {
    if(! serviceInitialized) {
      maltService.initializeParserModel(mkArgs(
        DEFAULT_MODEL_DIR, DEFAULT_MODEL_NAME,
        mkWorkDir("maltwdir", true)))
      serviceInitialized = true
    }
  }

  private def mkArgs(modelDir:String, modelName:String, workDir:String):String = {
    val args = new ArrayBuffer[String]()

    args += "-m"
    args += "parse"

    args += "-md"
    args += modelDir
    args += "-c"
    args += modelName

    args += "-w"
    args += workDir

    args += "-v"
    args += "error"

    args.mkString(" ")
  }

  private def mkWorkDir(prefix:String, deleteOnExit:Boolean):String = {
    val TEMP_DIR_ATTEMPTS = 100
    val baseDir = new File(System.getProperty("java.io.tmpdir"))
    val baseName = prefix + "-" + System.nanoTime().toString + "-"

    for(counter <- 0 until TEMP_DIR_ATTEMPTS) {
      val tempDir = new File(baseDir, baseName + counter.toString)
      if (tempDir.mkdir()) {
        if(deleteOnExit) tempDir.deleteOnExit()
        // println("work dir: " + tempDir.getAbsolutePath)
        return tempDir.getAbsolutePath
      }
    }

    throw new IllegalStateException("ERROR: Failed to create directory within "
      + TEMP_DIR_ATTEMPTS + " attempts (tried "
      + baseName + "0 to " + baseName + (TEMP_DIR_ATTEMPTS - 1) + ')');
  }

  override def resolveCoreference(doc:Document) {
    // FastNLP does not offer coreference resolution yet
  }
}

object FastNLPProcessor {
  val DEFAULT_MODEL_DIR = "models/"
  val DEFAULT_MODEL_NAME = "nivreeager-en-crammer"
}
