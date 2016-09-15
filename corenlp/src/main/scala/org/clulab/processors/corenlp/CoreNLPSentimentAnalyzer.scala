package org.clulab.processors.corenlp

import java.util.Properties

import org.clulab.processors.corenlp.CoreNLPUtils._
import org.clulab.processors.{Document, Sentence}
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.neural.rnn.RNNCoreAnnotations
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation

import scala.collection.JavaConverters._


object CoreNLPSentimentAnalyzer {

  private lazy val proc = new CoreNLPProcessor()
  private lazy val sentimentAnalyzer = mkSentimentAnalyzer

  /** Get a sentiment score from [[Document]] */
  def sentiment(doc: Document): Seq[Int] = for {
    s <- doc.sentences
  } yield sentiment(s)

  /** Create a corenlp pipeline for sentiment analysis */
  private def mkSentimentAnalyzer: StanfordCoreNLP = {
    val props = new Properties()
    // NOTE: CoreNLP requires parse trees to be "binarized"
    // in order to be used for sentiment analysis
    props.put("annotators", "binarizer, sentiment")
    props.put("customAnnotatorClass.binarizer", "edu.stanford.nlp.pipeline.BinarizerAnnotator")
    props.put("binarizer.tlppClass", "edu.stanford.nlp.parser.lexparser.EnglishTreebankParserParams")
    new StanfordCoreNLP(props, false)
  }

  /** Perform shallow analysis */
  private def mkDoc(text: String): Document = {
    val doc = proc.mkDocument(proc.preprocessText(text), keepText = false)
    proc.tagPartsOfSpeech(doc)
    doc.clear()
    doc
  }

  /** Get a sentiment score for a [[Sentence]] */
  def sentiment(s: Sentence): Int = {

    val a = sentenceToAnnotation(s)
    val sa = a.get(classOf[SentencesAnnotation]).asScala.toVector.head

    // needs to be a Stanford parse
    val tree = proc.stanfordParse(sa)
    sa.set(classOf[TreeAnnotation], tree)

    sentimentAnalyzer.annotate(a)

    val sentimentTree = sa.get(classOf[SentimentCoreAnnotations.AnnotatedTree])
    val score = RNNCoreAnnotations.getPredictedClass(sentimentTree)
    score
  }

  /**
   * Get a Sentiment score for each [[Sentence]] in a span of text
   * @param text a String
   */
  def sentiment(text: String): Seq[Int] = {
    // create doc using minimal calls,
    // so as to avoid parsing twice
    val doc = mkDoc(text)
    sentiment(doc)
  }
}
