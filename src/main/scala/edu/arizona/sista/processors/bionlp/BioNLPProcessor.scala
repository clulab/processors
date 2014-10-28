package edu.arizona.sista.processors.bionlp

import java.util.Properties

import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConversions._

/**
 * A processor for biomedical texts, based on CoreNLP, but with different tokenization and NER
 * User: mihais
 * Date: 10/27/14
 */
class BioNLPProcessor (internStrings:Boolean = true,
                       withDiscourse:Boolean = false)
  extends CoreNLPProcessor(internStrings, basicDependencies = false, withDiscourse) {

  override def mkTokenizerWithoutSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize")
    addBioOptions(props)
    new StanfordCoreNLP(props)
  }

  override def mkTokenizerWithSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize, ssplit")
    addBioOptions(props)
    new StanfordCoreNLP(props)
  }

  def addBioOptions(props:Properties) {
    props.put("tokenize.options", "ptb3Escaping=false")
  }

  /**
   * Implements the bio-specific post-processing steps from McClosky et al. (2011)
   * @param sentence Input CoreNLP sentence
   * @return The modified tokens
   */
  override def postprocessTokens(sentence:CoreMap): java.util.List[CoreLabel] = {
    val originalTokens = sentence.get(classOf[TokensAnnotation])

    val modifiedTokens = BioNLPTokenizer.postprocess(originalTokens)
    sentence.set(classOf[TokensAnnotation], modifiedTokens)

    modifiedTokens
  }

  override def recognizeNamedEntities(doc:Document) {
    val annotation = namedEntitySanityCheck(doc)
    if(annotation.isEmpty) return

    // run the NER on one sentence at a time
    // we are traversing our sentences and the CoreNLP sentences in parallel here!
    val sas = annotation.get.get(classOf[SentencesAnnotation])
    var sentenceOffset = 0
    for(sa:CoreMap <- sas) { // CoreNLP sentence annotation
      val s = doc.sentences(sentenceOffset) // our sentence
      val bioLabels = runBioNer(s)
      assert(bioLabels.size == s.size)

      // store labels in the CoreNLP annotation for the sentence
      val tas = sa.get(classOf[TokensAnnotation])
      var labelOffset = 0
      for(ta:CoreLabel <- tas) {
        ta.setNER(bioLabels(labelOffset))

        // TODO: we should have NormalizedNamedEntityTagAnnotation as well...

        labelOffset += 1
      }

      // store labels in our sentence
      s.entities = Some(bioLabels)
      // TODO: we should have s.norms as well...

      sentenceOffset += 1
    }
  }

  /**
   * Runs the bio-specific NER and returns an array of BIO (begin-input-output) labels for the sentence
   * @param sentence Our own sentence, containing words, lemmas, and POS tags
   * @return an array of BIO labels
   */
  def runBioNer(sentence:Sentence):Array[String] = {
    null // TODO: run Banner!
  }
}
