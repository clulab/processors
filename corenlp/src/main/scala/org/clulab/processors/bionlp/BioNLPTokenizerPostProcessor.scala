package org.clulab.processors.bionlp

import edu.stanford.nlp.ling.CoreLabel
import org.clulab.processors.clu.PostProcessorToken
import org.clulab.processors.clu.bio.BioTokenizerPostProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor

/**
 * Post processes CoreNLP tokenization so it suits bio analysis
 * User: mihais
 * Date: 11/16/14
 */
class BioNLPTokenizerPostProcessor(kbsWithTokensWithValidSlashes:Seq[String]) {

  val postProc = new BioTokenizerPostProcessor(kbsWithTokensWithValidSlashes)

  def process(input:Array[CoreLabel]):Array[PostProcessorToken] =
    postProc.process(ShallowNLPProcessor.coreLabelsToPostProcessorTokens(input))
}
