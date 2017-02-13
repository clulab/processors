package org.clulab.processors.bionlp

import java.util.Properties

import edu.stanford.nlp.pipeline.StanfordCoreNLP

/**
  * Basic utils shared between bio processors
  * User: mihais
  * Date: 2/9/17
  */
object BioNLPUtils {
  def mkTokenizerWithoutSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize")
    addBioTokenizerOptions(props)
    new StanfordCoreNLP(props)
  }

  def mkTokenizerWithSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize, ssplit")
    addBioTokenizerOptions(props)
    new StanfordCoreNLP(props)
  }

  def addBioTokenizerOptions(props:Properties) {
    props.put("tokenize.options", "ptb3Escaping=false")
    props.put("tokenize.language", "English")
  }
}
