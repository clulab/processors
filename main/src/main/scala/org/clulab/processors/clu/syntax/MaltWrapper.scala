package org.clulab.processors.clu.syntax

import java.io.File
import java.net.URL

import org.clulab.processors.Sentence
import org.clulab.struct.DirectedGraph
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.utils.Files
import org.maltparser.concurrent.{ConcurrentMaltParserModel, ConcurrentMaltParserService}
import org.maltparser.core.lw.helper.Utils

import MaltWrapper._

/**
  * A thin wrapper over the Malt parser
  * Works for both left-to-right and right-to-left parsing
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
    val parserModelName = Utils.getInternalParserModelName(modelURL)
    val path = modelURL.toString

    var url = modelURL
    if(path.startsWith("jar:")) {
      // we are already in a jar (we are in code external to processors)
      // extract the .mco models from this jar and store them on disk
      assert(path.startsWith("jar:file:") && path.endsWith(".mco"))
      val jarEnd = path.lastIndexOf("!/")
      val jarFileName = path.substring(9, jarEnd)
      val entryName = path.substring(jarEnd + 2)
      //println("JAR file: " + jarFileName)
      //println("Entry name: " + entryName)

      val tmpDir = Files.mkTmpDir("processors", deleteOnExit = true)
      val outFile = s"$tmpDir/$parserModelName.mco"
      Files.extractEntry(jarFileName, entryName, outFile, deleteOnExit = true)
      url = new URL(s"file:$outFile")
    }

    logger.debug(s"Using modelURL for parsing model $parserModelName: $url")
    ConcurrentMaltParserService.initializeParserModel(url)
  }

  /** Parses one sentence and creates the dependency graph for the resulting dependencies */
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
