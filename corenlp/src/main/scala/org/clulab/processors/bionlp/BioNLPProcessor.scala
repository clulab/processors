package org.clulab.processors.bionlp

import org.clulab.processors.bionlp.ner.{HybridNER, KBLoader}
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.processors.Document
import org.clulab.processors.corenlp.CoreNLPProcessor
import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import org.clulab.processors.clu.bio.{BioNERPostProcessor, BioTokenizerPreProcessor}

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
                       removeFigTabReferences:Boolean = true,
                       removeBibReferences:Boolean = true
)
  extends CoreNLPProcessor(internStrings, withChunks, withDiscourse, maxSentenceLength) {

  //lazy val banner = new BannerWrapper
  private lazy val postProcessor = new BioNLPTokenizerPostProcessor(KBLoader.UNSLASHABLE_TOKENS_KBS)
  private lazy val preProcessor = new BioTokenizerPreProcessor(removeFigTabReferences, removeBibReferences)
  private lazy val hybridNER = new HybridNER(withCRFNER, withRuleNER)
  private lazy val posPostProcessor = new BioNLPPOSTaggerPostProcessor
  private lazy val nerPostProcessor = new BioNERPostProcessor(KBLoader.stopListFile.get)

  override def mkTokenizerWithoutSentenceSplitting: StanfordCoreNLP = BioNLPUtils.mkTokenizerWithoutSentenceSplitting

  override def mkTokenizerWithSentenceSplitting: StanfordCoreNLP = BioNLPUtils.mkTokenizerWithSentenceSplitting

  override def postprocessTokens(originalTokens:Array[CoreLabel]):Array[CoreLabel] = postProcessor.process(originalTokens)

  override def preprocessText(origText:String):String = preProcessor.process(origText)

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

    for(sentence <- doc.sentences) {
      nerPostProcessor.process(sentence)
    }
  }

}

