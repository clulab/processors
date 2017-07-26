package org.clulab.processors.clulab.syntax

import org.clulab.processors.{Processor, Sentence}
import org.clulab.struct.{DirectedGraph, Edge}
import org.maltparser.concurrent.{ConcurrentMaltParserModel, ConcurrentMaltParserService}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import MaltWrapper._

/**
  * A thin wrapper over the Malt parser
  * User: mihais
  * Date: 7/11/17
  */
class MaltWrapper(val modelPath:String, val internStrings:Boolean = false) extends Parser {
  /**
    * One maltparser instance for each thread
    * MUST have one separate malt instance per thread!
    * malt uses a working directory which is written at runtime
    * using ThreadLocal variables guarantees that each thread gets its own working directory
    */
  lazy val maltModel:ConcurrentMaltParserModel = mkMaltModel(modelPath)

  def mkMaltModel(modelName:String): ConcurrentMaltParserModel = {
    val modelURL = MaltWrapper.getClass.getClassLoader.getResource(modelName)
    logger.debug(s"Using modelURL for parsing: $modelURL")
    //val parserModelName = Utils.getInternalParserModelName(modelURL)
    //logger.debug(s"parserModelName: $parserModelName")
    ConcurrentMaltParserService.initializeParserModel(modelURL)
  }

  override def getModel: ConcurrentMaltParserModel = maltModel

  /** Parses one sentence and stores the dependency graph in the sentence object */
  def parseSentence(sentence:Sentence):DirectedGraph[String] = {
    // tokens stores the tokens in the input format expected by malt (CoNLL-X)
    val inputTokens = new Array[String](sentence.words.length)
    for(i <- inputTokens.indices) {
      inputTokens(i) = s"${i + 1}\t${sentence.words(i)}\t${sentence.lemmas.get(i)}\t${sentence.tags.get(i)}\t${sentence.tags.get(i)}\t_"
    }

    // the actual parsing
    val outputTokens = maltModel.parseTokens(inputTokens)
    
    // convert malt's output into our dependency graph
    val edgeBuffer = new ListBuffer[Edge[String]]
    val roots = new mutable.HashSet[Int]
    for(o <- outputTokens) {
      //println(o)
      val tokens = o.split("\\s+")
      if(tokens.length < 8)
        throw new RuntimeException("ERROR: Invalid malt output line: " + o)
      // malt indexes tokens from 1; we index from 0
      val modifier = tokens(0).toInt - 1
      val head = tokens(6).toInt - 1
      val label = tokens(7).toLowerCase

      // sometimes malt generates dependencies from root with a different label than "root"
      // not sure why this happens, but let's manage this: create a root node in these cases
      if(head == -1) {
        roots += modifier
      } else {
        edgeBuffer += Edge(source = head, destination = modifier, relation = in(label))
      }
    }

    new DirectedGraph[String](edgeBuffer.toList, roots.toSet)
  }
  
  private def in(s:String):String = {
    if (internStrings) Processor.internString(s)
    else s
  }
}

object MaltWrapper {
  val logger = LoggerFactory.getLogger(classOf[MaltWrapper])

  val DEFAULT_FORWARD_MODEL_NAME = "org/clulab/processors/clu/en-forward-nivre.mco"
}
