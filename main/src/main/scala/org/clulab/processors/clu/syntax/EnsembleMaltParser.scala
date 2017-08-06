package org.clulab.processors.clu.syntax
import org.clulab.processors.Sentence
import org.clulab.struct.DirectedGraph

/**
  * Ensemble of several malt models
  * User: mihais
  * Date: 8/6/17
  */
class EnsembleMaltParser extends Parser {
  override def parseSentence(sentence: Sentence): DirectedGraph[String] = {

  }

  override def parseSentenceConllx(inputTokens: Array[String]): Array[String] = {
    
  }
}
