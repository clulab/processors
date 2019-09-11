package org.clulab.processors.bionlp

import org.clulab.processors.bionlp.ner.{HybridNER, KBLoader}
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.processors.Document
import org.clulab.processors.corenlp.CoreNLPProcessor
import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.pipeline.Annotation
import org.clulab.processors.bio.{BioNERPostProcessor, BioTokenizerPostProcessor}

import scala.collection.JavaConverters._

/**
 * A processor for biomedical texts, based on CoreNLP, but with different tokenization and NER
 * User: mihais
 * Date: 10/27/14
 * Last Modified: Update for Scala 2.12: java converters.
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
  extends CoreNLPProcessor(
    Some(new BioTokenizerPostProcessor(KBLoader.UNSLASHABLE_TOKENS_KBS)),
    internStrings, withChunks, withRelationExtraction = false, withDiscourse, maxSentenceLength) {

  //lazy val banner = new BannerWrapper
  lazy val hybridNER = new HybridNER(withCRFNER, withRuleNER)
  private lazy val posPostProcessor = new BioNLPPOSTaggerPostProcessor
  private lazy val nerPostProcessor = new BioNERPostProcessor(KBLoader.stopListFile.get)

  override def resolveCoreference(doc:Document): Unit = {
    doc.coreferenceChains = None
  }

  /**
   * Improve POS tagging in the Bio domain
   * @param annotation The CoreNLP annotation
   */
  override def postprocessTags(annotation:Annotation) {
    val sas = annotation.get(classOf[SentencesAnnotation]).asScala

    sas.foreach{ sa =>
      val tas = sa.get(classOf[TokensAnnotation]).asScala.toList.toArray
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

