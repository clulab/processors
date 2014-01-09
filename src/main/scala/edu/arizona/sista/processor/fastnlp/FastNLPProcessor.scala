package edu.arizona.sista.processor.fastnlp

import edu.arizona.sista.processor.corenlp.CoreNLPProcessor
import edu.arizona.sista.processor.{Sentence, Document}
import org.maltparserx.MaltParserService
import FastNLPProcessor._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import edu.arizona.sista.processor.utils.Files
import scala.collection.mutable
import edu.arizona.sista.processor.struct.DirectedGraph

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
      val dg = parseSentence(sentence)
      sentence.dependencies = Some(dg)
    }
  }

  /** Parses one sentence and stores the dependency graph in the sentence object */
  private def parseSentence(sentence:Sentence):DirectedGraph[String] = {
    // tokens stores the tokens in the input format expected by malt (CoNLL-X)
    val tokens = new Array[String](sentence.words.length)
    for(i <- 0 until tokens.length) {
      tokens(i) = s"${i + 1}\t${sentence.words(i)}\t${sentence.lemmas.get(i)}\t${sentence.tags.get(i)}\t${sentence.tags.get(i)}\t_"
    }

    // the actual parsing
    val output = maltService.parseTokens(tokens)

    // convert malt's output into our dependency graph
    val edgeBuffer = new ListBuffer[(Int, Int, String)]
    val roots = new mutable.HashSet[Int]
    for(o <- output) {
      //println(o)
      val tokens = o.split("\\s+")
      if(tokens.length < 8)
        throw new RuntimeException("ERROR: Invalid malt output line: " + o)
      // malt indexes tokens from 1; we index from 0
      val modifier = tokens(0).toInt - 1
      val head = tokens(6).toInt - 1
      val label = tokens(7).toLowerCase()
      if(label == "root" && head == -1) {
        roots += modifier
      } else {
        edgeBuffer += new Tuple3(head, modifier, in(label))
      }
    }
    new DirectedGraph[String](edgeBuffer.toList, roots.toSet)
  }

  private def initializeService() {
    if(! serviceInitialized) {
      maltService.initializeParserModel(mkArgs(
        DEFAULT_MODEL_DIR, DEFAULT_MODEL_NAME,
        Files.mkTmpDir("maltwdir", true)))
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

  override def resolveCoreference(doc:Document) {
    // FastNLP does not offer coreference resolution yet
  }
}

object FastNLPProcessor {
  val DEFAULT_MODEL_DIR = "models/"
  val DEFAULT_MODEL_NAME = "nivreeager-en-crammer"
}
