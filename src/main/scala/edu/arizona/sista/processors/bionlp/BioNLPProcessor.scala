package edu.arizona.sista.processors.bionlp

import java.util.Properties

import banner.BannerWrapper
import banner.tagging.Mention
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

  lazy val banner = new BannerWrapper

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
    val mentions = banner.tag(sentence.getSentenceText())

    val labels = new Array[String](sentence.size)
    for(i <- 0 until labels.size) labels(i) = "O"

    for(mention <- mentions) {
      alignMention(mention, sentence, labels)
    }

    labels
  }

  /**
   * Aligns a Banner Mention with the tokens in our Sentence
   * As a result, the labels get adjusted with the corresponding B- and I- labels from the Mention
   */
  private def alignMention(mention:Mention, sentence:Sentence, labels:Array[String]) {
    val (start, end) = matchMentionToTokens(mention, sentence)
    for(i <- start until end) {
      labels(i) = i match {
        case `start` => "B-" + mention.getType.toString
        case _ => "I-" + mention.getType.toString
      }
    }
  }

  private def matchMentionToTokens(mention:Mention, sentence:Sentence): (Int, Int) = {
    val start = mention.getStartChar + sentence.startOffsets.head
    val end = mention.getEndChar + sentence.startOffsets.head

    var startToken = -1
    var endToken = -1

    for(i <- 0 until sentence.size if endToken == -1) {
      if(startToken == -1 && tokenContains(sentence, i, start)) {
        startToken = i
      }
      if(startToken != -1 && tokenContains(sentence, i, end)) {
        endToken = i + 1
      }
    }

    if(startToken == -1 || endToken == -1) {
      throw new RuntimeException(s"ERROR: failed to match mention ($start, $end) to sentence: " + sentence.words.zip(sentence.startOffsets).mkString(", "))
    }

    (startToken, endToken)
  }

  private def tokenContains(sentence:Sentence, token:Int, charOffset:Int):Boolean =
    sentence.startOffsets(token) <= charOffset &&
    sentence.endOffsets(token) >= charOffset
}
