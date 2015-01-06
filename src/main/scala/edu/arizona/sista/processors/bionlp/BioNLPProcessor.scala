package edu.arizona.sista.processors.bionlp

import java.util.Properties
import java.util.regex.Pattern

import banner.BannerWrapper
import banner.tagging.Mention
import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

/**
 * A processor for biomedical texts, based on CoreNLP, but with different tokenization and NER
 * User: mihais
 * Date: 10/27/14
 */
class BioNLPProcessor (internStrings:Boolean = true,
                       withNER:Boolean = true,
                       withDiscourse:Boolean = false,
                       maxSentenceLength:Int = 100,
                       removeFigTabReferences:Boolean = true)
  extends CoreNLPProcessor(internStrings, basicDependencies = false, withDiscourse, maxSentenceLength) {

  lazy val banner = new BannerWrapper
  lazy val postProcessor = new BioNLPTokenizerPostProcessor

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
  }

  /**
   * Implements the bio-specific post-processing steps from McClosky et al. (2011)
   * @param sentence Input CoreNLP sentence
   * @return The modified tokens
   */
  override def postprocessTokens(sentence:CoreMap): java.util.List[CoreLabel] = {
    val originalTokens = sentence.get(classOf[TokensAnnotation])

    val modifiedTokens = postProcessor.process(originalTokens.asScala.toArray).toList.asJava
    sentence.set(classOf[TokensAnnotation], modifiedTokens)

    modifiedTokens
  }

  /**
   * Removes Figure and Table references that appear within parentheses
   * @param origText The original input text
   * @return The preprocessed text
   */
  override def preprocessText(origText:String):String = {
    if (!removeFigTabReferences) return origText

    var noRefs = origText
    // the pattern with parens must run first!
    noRefs = removeFigTabRefs(BioNLPProcessor.FIGTAB_REFERENCE_WITH_PARENS, noRefs)
    noRefs = removeFigTabRefs(BioNLPProcessor.FIGTAB_REFERENCE, noRefs)
    noRefs
  }

  def removeFigTabRefs(pattern:Pattern, text:String):String = {
    val m = pattern.matcher(text)
    val b = new StringBuilder
    var previousEnd = 0
    while(m.find()) {
      b.append(text.substring(previousEnd, m.start()))
      // white out the reference, keeping the same number of characters
      for(i <- m.start() until m.end()) b.append(" ")
      previousEnd = m.end()
    }
    if(previousEnd < text.size)
      b.append(text.substring(previousEnd))
    b.toString()
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

      //println("Running NER on a sentence of length " + s.size)
      val bioLabels = runBioNer(s)
      assert(bioLabels.size == s.size)
      //println("\t--> done.")

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
    val labels = new Array[String](sentence.size)
    for(i <- 0 until labels.size) labels(i) = "O"
    if(! withNER) return labels // do not run the NER

    //println("RUNNING BANNER ON SENTENCE: " + sentence.words.mkString(" "))

    try {
      val mentions = banner.tag(sentence.getSentenceText())

      for (mention <- mentions) {
        alignMention(mention, sentence, labels)
      }
    } catch {
      case e:MatchException =>
        throw e // this is bad, so rethrow
      case e:Throwable => // anything else is a bug in Banner
        System.err.println(s"WARNING: BANNER failed with message ${e.getMessage}}. Continuing...")
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

  class MatchException(s:String) extends RuntimeException(s)

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
      throw new MatchException(s"ERROR: failed to match mention ($start, $end) to sentence: " + sentence.words.zip(sentence.startOffsets).mkString(", "))
    }

    (startToken, endToken)
  }

  private def tokenContains(sentence:Sentence, token:Int, charOffset:Int):Boolean =
    sentence.startOffsets(token) <= charOffset &&
    sentence.endOffsets(token) >= charOffset

  override def resolveCoreference(doc:Document): Unit = {
    // TODO: add domain-specific coreference here!
    doc.coreferenceChains = None
  }
}

object BioNLPProcessor {
  val FIGTAB_REFERENCE_WITH_PARENS = Pattern.compile("\\((\\s*see)?\\s*(figure|table|fig\\.|tab\\.)[^\\)]*\\)", Pattern.CASE_INSENSITIVE)
  val FIGTAB_REFERENCE = Pattern.compile("(\\s*see)?\\s*(figure|table|fig\\.|tab\\.)\\s*[0-9A-Za-z\\.]+", Pattern.CASE_INSENSITIVE)
}

