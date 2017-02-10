package org.clulab.processors.bionlp

import java.util.Properties

import org.clulab.processors.bionlp.ner.{HybridNER, KBLoader}
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.processors.Document
import org.clulab.processors.corenlp.CoreNLPProcessor
import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import org.slf4j.LoggerFactory

import scala.collection.JavaConversions._

/**
 * A processor for biomedical texts, based on CoreNLP, but with different tokenization and NER
 * User: mihais
 * Date: 10/27/14
 */
class BioNLPProcessor (internStrings:Boolean = false,
                       withChunks:Boolean = true,
                       withCRFNER:Boolean = true,
                       withRuleNER:Boolean = true,
                       withContext:Boolean = true,
                       withDiscourse:Int = ShallowNLPProcessor.NO_DISCOURSE,
                       maxSentenceLength:Int = 100,
                       removeFigTabReferences:Boolean = true)
  extends CoreNLPProcessor(internStrings, withChunks, withDiscourse, maxSentenceLength) {

  //lazy val banner = new BannerWrapper
  lazy val specialTokens = KBLoader.loadSpecialTokens
  lazy val postProcessor = new BioNLPTokenizerPostProcessor(specialTokens)
  lazy val preProcessor = new BioNLPPreProcessor(removeFigTabReferences)
  lazy val hybridNER = new HybridNER(withCRFNER, withRuleNER)
  lazy val posPostProcessor = new BioNLPPOSTaggerPostProcessor

  override def mkTokenizerWithoutSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize")
    addBioTokenizerOptions(props)
    new StanfordCoreNLP(props)
  }

  override def mkTokenizerWithSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize, ssplit")
    addBioTokenizerOptions(props)
    new StanfordCoreNLP(props)
  }

  def addBioTokenizerOptions(props:Properties) {
    props.put("tokenize.options", "ptb3Escaping=false")
    props.put("tokenize.language", "English")
  }

  /**
   * Implements the bio-specific post-processing steps from McClosky et al. (2011)
 *
   * @param originalTokens Input CoreNLP sentence
   * @return The modified tokens
   */
  override def postprocessTokens(originalTokens:Array[CoreLabel]) = postProcessor.process(originalTokens)

  override def preprocessText(origText:String):String = preProcessor.preprocess(origText)

  override def resolveCoreference(doc:Document): Unit = {
    doc.coreferenceChains = None
  }

  /**
   * Improve POS tagging in the Bio domain
   * @param annotation The CoreNLP annotation
   */
  override def postprocessTags(annotation:Annotation) {
    val sas = annotation.get(classOf[SentencesAnnotation])

    sas.foreach{ sa =>
      val tas = sa.get(classOf[TokensAnnotation]).toList.toArray
      posPostProcessor.postprocessCoreLabelTags(tas)
    }
  }

  override def recognizeNamedEntities(doc:Document) {
    hybridNER.recognizeNamedEntities(doc, namedEntitySanityCheck(doc))
  }

}

object BioNLPProcessor {
  val logger = LoggerFactory.getLogger(classOf[BioNLPProcessor])
}
