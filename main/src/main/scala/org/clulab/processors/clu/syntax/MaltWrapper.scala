package org.clulab.processors.clu.syntax

import java.io.File

import org.clulab.processors.Sentence
import org.clulab.struct.DirectedGraph
import org.maltparser.concurrent.{ConcurrentMaltParserModel, ConcurrentMaltParserService}
import org.slf4j.{Logger, LoggerFactory}

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

  /**
    * If the model name contains "backward", we flag this parsing model as right-to-left
    */
  val isRightToLeft:Boolean = detectReverse(modelPath)

  def detectReverse(modelPath: String):Boolean = {
    val name = new File(modelPath).getName
    if(name.contains("backward")) true
    else false
  }

  def mkMaltModel(modelName:String): ConcurrentMaltParserModel = {
    val modelURL = MaltWrapper.getClass.getClassLoader.getResource(modelName)
    logger.debug(s"Using modelURL for parsing: $modelURL")
    //val parserModelName = Utils.getInternalParserModelName(modelURL)
    //logger.debug(s"parserModelName: $parserModelName")
    ConcurrentMaltParserService.initializeParserModel(modelURL)
  }

  /** Parses one sentence and stores the dependency graph in the sentence object */
  override def parseSentence(sentence:Sentence):DirectedGraph[String] = {
    // tokens stores the tokens in the input format expected by malt (CoNLL-X)
    val inputTokens = MaltUtils.sentenceToConllx(sentence)

    // the actual parsing
    val outputTokens = parseSentenceConllx(inputTokens)

    // convert malt's output into our dependency graph
    MaltUtils.conllxToDirectedGraph(outputTokens, internStrings)
  }

  override def parseSentenceConllx(inputTokens: Array[String]):Array[String] = {
    val actualTokens = if(isRightToLeft) ReverseTreebank.revertSentence(inputTokens) else inputTokens
    val outputTokens = maltModel.parseTokens(actualTokens)
    if(isRightToLeft) ReverseTreebank.revertSentence(outputTokens)
    else outputTokens
  }
  
}

object MaltWrapper {
  val logger: Logger = LoggerFactory.getLogger(classOf[MaltWrapper])
}
