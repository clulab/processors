package edu.arizona.sista.processors.bionlp.ner

import java.util
import java.util.Properties

import edu.arizona.sista.processors.{Sentence, Processor}
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.utils.StringUtils
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.ling.CoreAnnotations.AnswerAnnotation
import edu.stanford.nlp.ling.CoreLabel
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import java.util.{List => JavaList}

import org.slf4j.LoggerFactory
import BioNER._

import scala.collection.mutable.ListBuffer
import scala.io.StdIn


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
    val crf = new CRFClassifier[CoreLabel](props)
    crf
  }

  def train(path:String) = {
    crfClassifier = Some(mkClassifier())
    val trainCorpus = readData(path, BioNER.USE_IO)
    crfClassifier.foreach(_.train(trainCorpus))
  }

  def save(path:String) { crfClassifier.foreach(_.serializeClassifier(path)) }

  /**
   * Classifies a sentence in the Stanford format
   * @param sentence Input sentence; each token must contain: word, lemma, POS tag
   * @return The IOB predictions for this sentence
   */
  def classify(sentence:JavaList[CoreLabel]):List[String] = {
    assert(crfClassifier.isDefined)
    val labels = new ListBuffer[String]
    val predictions = crfClassifier.get.classify(sentence)
    for(l <- predictions) {
      labels += reverseNormalize(l.getString(classOf[AnswerAnnotation]))
    }
    if(BioNER.USE_IO) ioToIob(labels.toList)
    labels.toList
  }

  def test(path:String): List[List[(String, String)]] = {
    val testCorpus = readData(path, convertToIOFormat = false)
    val outputs = new ListBuffer[List[(String, String)]]
    for(sentence <- testCorpus) {
      val golds = fetchGoldLabels(sentence.asScala.toList)
      val preds = classify(sentence).toArray
      outputs += golds.zip(preds)
    }
    outputs.toList
  }
}

object BioNER {
  val logger = LoggerFactory.getLogger(classOf[BioNER])

  val USE_IO = false

  /** Reads IOB data directly into Java lists, because the CRF needs the data of this type */
  def readData(path: String, convertToIOFormat: Boolean): JavaList[JavaList[CoreLabel]] = {
    val sentences = new util.ArrayList[JavaList[CoreLabel]]()
    var crtSentence = new util.ArrayList[CoreLabel]()
    var totalTokens = 0
    for (line <- io.Source.fromFile(path).getLines()) {
      val trimmed = line.trim
      if (trimmed.isEmpty) {
        if (crtSentence.size() > 0) {
          sentences.add(crtSentence)
          crtSentence = new util.ArrayList[CoreLabel]()
        }
      } else {
        crtSentence.add(mkCoreLabel(trimmed, convertToIOFormat))
        totalTokens += 1
      }
    }
    logger.info(s"In file $path I found ${sentences.size} sentences with an average of ${totalTokens / sentences.size} words/sentence.")
    sentences
  }

  def mkCoreLabel(line: String, convertToIOFormat: Boolean): CoreLabel = {
    val l = new CoreLabel()
    val bits = line.split("\\s+") // robustSplit(line, 3)
    assert(bits.length == 4)
    l.setWord(bits(0))
    l.setTag(bits(1))
    l.setLemma(bits(2))

    val label = normalizeLabel(bits(3), convertToIOFormat)
    l.setNER(label)
    l.set(classOf[AnswerAnnotation], label)
    l
  }

  def normalizeLabel(l: String, convertToIOFormat: Boolean): String = l match {
    case "B-Gene_or_gene_product" => if (convertToIOFormat) "I-GENE" else "B-GENE"
    case "I-Gene_or_gene_product" => "I-GENE"
    case _ => "O"
  }

  def reverseNormalize(l: String) =
    l match {
      case RuleNER.OUTSIDE_LABEL => RuleNER.OUTSIDE_LABEL
      case _ => BioNLPProcessor.NORMALIZED_LABELS.get(l).get // TODO: we should remove normalizeLabel and reverseNormalize
    }

  def ioToIob(labels:List[String]):List[String] = {
    val converted = new ListBuffer[String]
    var prev:String = null
    for(label <- labels) {
      if(label.startsWith("I-") && prev != null && prev != label)
        converted += "B-" + label.substring(2)
      else
        converted += label
      prev = label
    }
    converted.toList
  }

  /** Splits a line into k tokens, knowing that the left-most one might contain spaces */
  def robustSplit(line:String, k:Int):Array[String] = {
    val bits = new ListBuffer[String]
    var pos = line.length - 1
    for(i <- 0 until k - 1) {
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

    if(props.containsKey("shell")) {
      assert(props.containsKey("model"))
      val ner = load(props.getProperty("model"))
      shell(ner)
    }
  }

  def shell(ner:BioNER) {
    val proc:Processor = new BioNLPProcessor(withDiscourse = false, removeFigTabReferences = true)
    while(true) {
      print("> ")
      val text = StdIn.readLine()
      val doc = proc.annotate(text)

      for(sentence <- doc.sentences) {
        evalSent(ner, sentence)
      }
    }
  }

  def evalSent(ner:BioNER, sentence:Sentence) {
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
