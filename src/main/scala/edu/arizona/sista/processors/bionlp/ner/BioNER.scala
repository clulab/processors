package edu.arizona.sista.processors.bionlp.ner

import java.util
import java.util.Properties

import edu.arizona.sista.utils.StringUtils
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.ling.CoreAnnotations.AnswerAnnotation
import edu.stanford.nlp.ling.CoreLabel
import scala.collection.JavaConversions._

import java.util.{List => JavaList}

import org.slf4j.LoggerFactory
import BioNER._

import scala.collection.mutable.ListBuffer


/**
 * Our own BIO NER trained on the BioCreative 2 dataset, using the Stanford CRF
 * User: mihais
 * Date: 2/27/15
 */
class BioNER {
  var crfClassifier:Option[CRFClassifier[CoreLabel]] = None

  private def mkClassifier(): CRFClassifier[CoreLabel] = {
    val props = new Properties()
    props.setProperty("macro", "true")
    props.setProperty("featureFactory", "edu.arizona.sista.processors.bionlp.ner.BioNERFactory")
    //props.setProperty("l1reg", "0.1"); // for L1 regularization
    val crf = new CRFClassifier[CoreLabel](props)
    crf
  }

  def train(path:String) = {
    crfClassifier = Some(mkClassifier())
    val trainCorpus = readData(path)
    crfClassifier.foreach(_.train(trainCorpus))
  }

  def save(path:String) { crfClassifier.foreach(_.serializeClassifier(path)) }

  /** Reads IOB data directly into Java lists, because the CRF needs the data of this type */
  def readData(path:String):JavaList[JavaList[CoreLabel]] = {
    val sentences = new util.ArrayList[JavaList[CoreLabel]]()
    var crtSentence = new util.ArrayList[CoreLabel]()
    var totalTokens = 0
    for(line <- io.Source.fromFile(path).getLines()) {
      val trimmed = line.trim
      if(trimmed.isEmpty) {
        if(crtSentence.size() > 0) {
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

  def mkCoreLabel(line:String):CoreLabel = {
    val l = new CoreLabel()
    //println(s"\tLINE: [$line]")
    val bits = robustSplit(line)
    //println(s"\tBITS ${bits.mkString(" ")}")
    assert(bits.length == 3)
    l.setWord(bits(0))
    l.setTag(bits(1))
    l.setNER(bits(2))
    l.set(classOf[AnswerAnnotation], bits(2))
    l
  }

  /** Splits a line into 3 tokens, knowing that the first one might contain spaces */
  def robustSplit(line:String):Array[String] = {
    val bits = new ListBuffer[String]
    var pos = line.size - 1
    for(i <- 0 until 2) {
      val newPos = line.lastIndexOf(' ', pos)
      assert(newPos > 0)
      val bit = line.substring(newPos + 1, pos + 1)
      bits.insert(0, bit)
      pos = newPos - 1
    }
    bits.insert(0, line.substring(0, pos + 1))
    bits.toArray
  }

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
      val preds = classify(sentence).toArray
      val sentOutputs = new ListBuffer[(String, String)]
      for(i <- 0 until preds.size) {
        val gold = sentence.get(i).getString(classOf[AnswerAnnotation])
        val sys = preds(i)
        sentOutputs += new Tuple2(gold, sys)
      }
      outputs += sentOutputs.toList
    }
    outputs.toList
  }
}

object BioNER {
  val logger = LoggerFactory.getLogger(classOf[BioNER])

  def load(path:String):BioNER = {
    val ner = new BioNER
    ner.crfClassifier = Some(ner.mkClassifier())
    ner.crfClassifier.get.loadClassifier(path)
    ner
  }

  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)

    if(props.containsKey("train")) {
      val ner = new BioNER
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
  }
}
