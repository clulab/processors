package org.clulab.processors.bionlp

import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.process.CoreLabelTokenFactory
import org.clulab.processors.clu.bio.{BioTokenizerPostProcessor, PostProcessorToken}

/**
 * Post processes CoreNLP tokenization so it suits bio analysis
 * User: mihais
 * Date: 11/16/14
 */
class BioNLPTokenizerPostProcessor(kbsWithTokensWithValidSlashes:Seq[String]) {

  val postProc = new BioTokenizerPostProcessor(kbsWithTokensWithValidSlashes)

  def process(input:Array[CoreLabel]):Array[CoreLabel] = {
    toCoreLabels(postProc.process(toPostProcessorTokens(input)))
  }

  private def toPostProcessorTokens(labels: Array[CoreLabel]): Array[PostProcessorToken] = {
    val tokens = new Array[PostProcessorToken](labels.length)
    for(i <- labels.indices) {
      tokens(i) = PostProcessorToken(labels(i).word(), labels(i).beginPosition(), labels(i).endPosition())
    }
    tokens
  }

  private def toCoreLabels(tokens: Array[PostProcessorToken]): Array[CoreLabel] = {
    val labels = new Array[CoreLabel](tokens.length)
    val f = new CoreLabelTokenFactory()
    for(i <- tokens.indices) {
      val l = f.makeToken(
        tokens(i).word,
        tokens(i).beginPosition,
        tokens(i).endPosition - tokens(i).beginPosition)
      labels(i) = l
    }
    labels
  }
}
