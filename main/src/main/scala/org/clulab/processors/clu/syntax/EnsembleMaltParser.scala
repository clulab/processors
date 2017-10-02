package org.clulab.processors.clu.syntax
import org.clulab.processors.Sentence
import org.clulab.struct.DirectedGraph

import scala.collection.mutable.ArrayBuffer

/**
  * Ensemble of several malt models
  * User: mihais
  * Date: 8/6/17
  */
class EnsembleMaltParser(val modelPaths:Seq[String], val internStrings:Boolean = false) extends Parser {

  lazy val maltModels: Array[Parser] = mkModels(modelPaths, internStrings)

  def mkModels(modelPaths:Seq[String], internStrings:Boolean):Array[Parser] = {
    val models = new ArrayBuffer[Parser]()
    for(modelPath <- modelPaths)
      models += new MaltWrapper(modelPath, internStrings)
    models.toArray
  }

  override def parseSentence(sentence: Sentence): DirectedGraph[String] = {
    // tokens stores the tokens in the input format expected by malt (CoNLL-X)
    val inputTokens = MaltUtils.sentenceToConllx(sentence)

    // the actual parsing
    val dg = ensembleParse(inputTokens)

    dg
  }

  def ensembleParse(inputTokens: Array[String]):DirectedGraph[String] = {
    val parses = new ArrayBuffer[DirectedGraph[String]]()
    for(model <- maltModels) {
      val parse = model.parseSentenceConllx(inputTokens)
      parses += MaltUtils.conllxToDirectedGraph(parse, internStrings)
    }

    val eisner = new EnsembleModel(parses.toArray)
    eisner.parse()
  }

  override def parseSentenceConllx(inputTokens: Array[String]): Array[String] = {
    val dg = ensembleParse(inputTokens)
    MaltUtils.directedGraphToConllx(dg, inputTokens)
  }
}
