package org.clulab.processors.bionlp.ner

import java.util
import java.util.Properties
import java.util.{List => JavaList}

import org.slf4j.LoggerFactory

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.io.{ Source, StdIn }

import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.ling.CoreAnnotations.AnswerAnnotation
import edu.stanford.nlp.ling.CoreLabel

import org.clulab.processors.{Processor, Sentence}
import org.clulab.processors.bionlp.{BioNLPPOSTaggerPostProcessor, BioNLPProcessor}
import org.clulab.sequences.SeqScorer
import org.clulab.utils.StringUtils

import CRFNER._

/**
 * Our own BIO NER trained on the BioCreative 2 dataset, using the Stanford CRF
 * User: mihais
 * Date: 2/27/15
 * Last Modified: Fix compiler issue: import scala.io.Source.
 */
class CRFNER {
  var crfClassifier:Option[CRFClassifier[CoreLabel]] = None

  private def mkClassifier(): CRFClassifier[CoreLabel] = {
    val props = new Properties()
    props.setProperty("macro", "true")
    props.setProperty("featureFactory", "org.clulab.processors.bionlp.ner.BioNERFactory")
    val crf = new CRFClassifier[CoreLabel](props)
    crf
  }

  def train(path:String) {
    crfClassifier = Some(mkClassifier())
    val trainCorpus = readData(path)
    crfClassifier.foreach(_.train(trainCorpus))
  }

  def save(path:String) { crfClassifier.foreach(_.serializeClassifier(path)) }

  /**
   * Classifies a sentence in the Stanford format
 *
   * @param sentence Input sentence; each token must contain: word, lemma, POS tag
   * @return The IOB predictions for this sentence
   */
  def classify(sentence:JavaList[CoreLabel]):List[String] = {
    assert(crfClassifier.isDefined)
    val labels = new ListBuffer[String]
    val predictions = crfClassifier.get.classify(sentence)
    for(l <- predictions) {
      labels += l.getString(classOf[AnswerAnnotation])
    }
    labels.toList
  }

  def test(path:String): List[List[(String, String)]] = {
    val testCorpus = readData(path)
    val outputs = new ListBuffer[List[(String, String)]]
    for(sentence <- testCorpus) {
      val golds = fetchGoldLabels(sentence.asScala.toList)
      val preds = classify(sentence).toArray
      val outputPerSent = golds.zip(preds)
      outputs += outputPerSent

      // println(outputPerSent)
    }
    outputs.toList
  }
}

object CRFNER {
  private val logger = LoggerFactory.getLogger(classOf[CRFNER])
  private val posPostProcessor = new BioNLPPOSTaggerPostProcessor

  /** Reads IOB data directly into Java lists, because the CRF needs the data of this type */
  def readData(path: String): JavaList[JavaList[CoreLabel]] = {
    val sentences = new util.ArrayList[JavaList[CoreLabel]]()
    var crtSentence = new util.ArrayList[CoreLabel]()
    var totalTokens = 0
    for (line <- Source.fromFile(path).getLines()) {
      val trimmed = line.trim
      if (trimmed.isEmpty) {
        if (crtSentence.size() > 0) {
          postProcessTags(crtSentence)
          sentences.add(crtSentence)
          crtSentence = new util.ArrayList[CoreLabel]()
        }
      } else {
        crtSentence.add(mkCoreLabel(trimmed))
        totalTokens += 1
      }
    }
    logger.info(s"In file $path I found ${sentences.size} sentences with an average of ${totalTokens / sentences.size} words/sentence.")
    sentences
  }

  def mkCoreLabel(line: String): CoreLabel = {
    val l = new CoreLabel()
    val bits = line.split("\\s+") // robustSplit(line, 3)
    assert(bits.length == 4)
    l.setOriginalText(bits(0))
    l.setWord(bits(0))
    l.setTag(bits(1))
    l.setLemma(bits(2))

    val label = bits(3)
    l.setNER(label)
    l.set(classOf[AnswerAnnotation], label)
    l
  }

  /**
    * Fixes common POS tagging mistakes, using the same code used by BioNLPProcessor at runtime
    *
    * @param sentence List of tokens in one sentence
    */
  def postProcessTags(sentence:JavaList[CoreLabel]): Unit = {
    val tokens = new Array[CoreLabel](sentence.size())
    for(i <- 0 until sentence.size()) {
      tokens(i) = sentence.get(i)
    }

    posPostProcessor.postprocessCoreLabelTags(tokens)
  }

  /** Splits a line into k tokens, knowing that the left-most one might contain spaces */
  def robustSplit(line:String, k:Int):Array[String] = {
    val bits = new ListBuffer[String]
    var pos = line.length - 1
    for(_ <- 0 until k - 1) {
      val newPos = line.lastIndexOf(' ', pos)
      assert(newPos > 0)
      val bit = line.substring(newPos + 1, pos + 1)
      bits.insert(0, bit)
      pos = newPos - 1
    }
    bits.insert(0, line.substring(0, pos + 1))
    bits.toArray
  }

  def fetchGoldLabels(sentence:List[CoreLabel]):List[String] = {
    val golds = sentence.map(_.ner())

    // reset all gold labels to O so they are not visible at testing time
    sentence.foreach(t => {
      t.setNER("O")
      t.set(classOf[AnswerAnnotation], "O")
    })
    golds
  }

  def load(path:String):CRFNER = {
    val ner = new CRFNER
    ner.crfClassifier = Some(ner.mkClassifier())
    ner.crfClassifier.get.loadClassifier(path)
    ner
  }

  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)

    if(props.containsKey("train")) {
      val ner = new CRFNER
      ner.train(props.getProperty("train"))
      if(props.containsKey("model")) {
        ner.save(props.getProperty("model"))
      }
    }

    if(props.containsKey("test")) {
      assert(props.containsKey("model"))
      val ner = load(props.getProperty("model"))
      val outputs = ner.test(props.getProperty("test"))
      val scorer = new SeqScorer
      scorer.score(outputs)
    }

    if(props.containsKey("shell")) {
      assert(props.containsKey("model"))
      val ner = load(props.getProperty("model"))
      shell(ner)
    }
  }

  def shell(ner:CRFNER) {
    val proc:Processor = new BioNLPProcessor()
    while(true) {
      print("> ")
      val text = StdIn.readLine()
      val doc = proc.annotate(text)

      for(sentence <- doc.sentences) {
        evalSent(ner, sentence)
      }
    }
  }

  def evalSent(ner:CRFNER, sentence:Sentence) {
    println("Evaluating sentence: " + sentence.words.mkString(" "))
    val tokens = new util.ArrayList[CoreLabel]()
    for(i <- 0 until sentence.size) {
      val l = new CoreLabel()
      l.setWord(sentence.words(i))
      l.setTag(sentence.tags.get(i))
      tokens.add(l)
    }
    val preds = ner.classify(tokens)
    println(preds)
  }
}
