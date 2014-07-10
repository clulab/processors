package edu.arizona.sista.processors.fastnlp

import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.struct.DirectedGraph
import edu.arizona.sista.utils.Files
import org.maltparserx.MaltParserService
import FastNLPProcessor._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.collection.mutable
import org.maltparserx

/**
 * Fast NLP tools
 * Uses most of CoreNLP but replaces its parser with maltparser
 * This means that constituent trees and coreference, which depends on that, are not available
 * Also, malt produces Stanford "basic" dependencies, rather than "collapsed" ones
 * User: mihais
 * Date: 1/4/14
 */
class FastNLPProcessor(internStrings:Boolean = true) extends CoreNLPProcessor(internStrings) {
  /**
   * One maltparser instance for each thread
   * MUST have one separate malt instance per thread!
   * malt uses a working directory which is written at runtime
   * using ThreadLocal variables guarantees that each thread gets its own working directory
   */
  private val maltService = new ThreadLocal[MaltParserService]

  override def parse(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return
    if (doc.sentences.head.tags == None)
      throw new RuntimeException("ERROR: you have to run the POS tagger before NER!")
    if (doc.sentences.head.lemmas == None)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before NER!")

    // parse each individual sentence
    val debug = false
    for(sentence <- doc.sentences) {
      if(debug) {
        print("PARSING SENTENCE:")
        for(i <- 0 until sentence.size) print(" " + sentence.words(i))
        println()
      }
      val dg = parseSentence(sentence)
      sentence.dependencies = Some(dg)
      if(debug) {
        println("DONE.")
      }
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
    val output = getService().parseTokens(tokens)

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

      // sometimes malt generates dependencies from root with a different label than "root"
      // not sure why this happens, but let's manage this: create a root node for all
      // if(label == "root" && head == -1) {
      if(head == -1) {
        roots += modifier
      } else {
        edgeBuffer += new Tuple3(head, modifier, in(label))
      }
    }

    new DirectedGraph[String](edgeBuffer.toList, roots.toSet)
  }

  private def getService():MaltParserService = {
    if(maltService.get() == null) {
      val service = new maltparserx.MaltParserService()
      service.initializeParserModel(mkArgs(
        Files.mkTmpDir("maltwdir", deleteOnExit = true),
        DEFAULT_MODEL_NAME))
      maltService.set(service)
    }
    maltService.get()
  }

  private def mkArgs(workDir:String, modelName:String):String = {
    val args = new ArrayBuffer[String]()

    args += "-m"
    args += "parse"

    args += "-w"
    args += workDir

    args += "-c"
    args += modelName

    args += "-v"
    args += "error"

    args.mkString(" ")
  }

  override def resolveCoreference(doc:Document) {
    // FastNLP does not offer coreference resolution yet
  }
}

object FastNLPProcessor {
  val DEFAULT_MODEL_NAME = "nivreeager-en-crammer"
}
