package org.clulab.processors.clu.sequences

import java.io._

import org.clulab.learning._
import org.clulab.processors.{Document, Sentence}

import scala.collection.mutable.ArrayBuffer
import SequenceTaggerLogger._
import org.clulab.struct.Counter

/**
  * Bidirectional MEMM sequence tagger
  * User: mihais
  * Date: 8/27/17
  */
abstract class BiMEMMSequenceTagger[L, F](
  var order:Int = 1,
  val numFoldsFirstPass:Int = 3,
  val leftToRightFirstPass:Boolean = false,
  val leftToRightSecondPass:Boolean = true) extends SequenceTagger[L, F] {
  var firstPassModel:Option[Classifier[L, F]] = None
  var secondPassModel:Option[Classifier[L, F]] = None

  override def train(docs:Iterator[Document]): Unit = {
    val sentences = new ArrayBuffer[Sentence]()
    for(doc <- docs; sent <- doc.sentences) {
      sentences += sent
    }

    logger.debug(s"Training on ${sentences.size} sentences.")
    logger.debug(s"Generating features using order $order...")
    val folds = Datasets.mkFolds(numFoldsFirstPass, sentences.size)
    logger.debug(s"Using $numFoldsFirstPass folds for the first pass model.")

    // create dataset for the second pass model
    // this creates a different first pass model for each fold
    val dataset = mkDataset
    var foldCount = 1
    for(fold <- folds) {
      logger.debug(s"In fold $foldCount: ${fold.testFold}...")
      foldCount += 1

      val firstPass = mkFirstPassModelOnFold(fold, sentences)
      addToDataset(dataset, sentences, fold.testFold, Some(firstPass), leftToRight = leftToRightSecondPass)
    }
    logger.debug("Finished processing all sentences for the second pass model.")

    // train second pass model
    val classifier = mkClassifier
    logger.debug("Started training the second pass classifier...")
    classifier.train(dataset)
    secondPassModel = Some(classifier)
    logger.debug("Finished training the second pass model.")

    // create dataset for the first pass model to be used in testing
    logger.debug("Preparing dataset for the complete first pass model...")
    val firstPassDataset = mkDataset
    addToDataset(firstPassDataset, sentences, Tuple2(0, sentences.size), None, leftToRight = leftToRightFirstPass)
    logger.debug("Finished processing all sentences for the first pass model.")
    // train the first pass model to be used in testing
    val firstPassClassifier = mkClassifier
    logger.debug("Started training the first pass classifier...")
    firstPassClassifier.train(dataset)
    firstPassModel = Some(firstPassClassifier)
    logger.debug("Finished training the first pass model.")

  }

  def addToDataset(
    dataset: Dataset[L, F],
    sentences: ArrayBuffer[Sentence],
    fold: (Int, Int),
    firstPass: Option[Classifier[L, F]],
    leftToRight: Boolean): Unit = {
    for(sentOffset <- fold._1 until fold._2) {
      val sentence = if(leftToRight) sentences(sentOffset) else sentences(sentOffset).revert()

      val features = new Array[Counter[F]](sentence.size)
      for(i <- features.indices) features(i) = new Counter[F]()

      // labels and features for one sentence
      val labels = labelExtractor(sentence)
      (0 until sentence.size).map(i => featureExtractor(features(i), sentence, i))

      // add history features: concatenate the labels of the previous <order> tokens to the features
      for(i <- features.indices) {
        addHistoryFeatures(features(i), order, labels, i)
      }

      // add first pass features: the label predicted by the first pass model
      if(firstPass.nonEmpty) {
        val firstPassLabels = predict(firstPass.get, sentence, None, leftToRightFirstPass)
        for(i <- features.indices) {
          addFirstPassFeatures(features(i), order, firstPassLabels, i)
        }
      }

      // add to dataset
      for(i <- features.indices) {
        val d = mkDatum(labels(i), features(i))
        dataset += d
      }
    }
  }

  def mkFirstPassModelOnFold(fold: DatasetFold, sentences: ArrayBuffer[Sentence]): Classifier[L, F] = {
    logger.debug("Training first pass model for this fold...")
    val dataset = mkDataset
    for(tf <- fold.trainFolds) {
      addToDataset(dataset, sentences, tf, None, leftToRight = leftToRightFirstPass)
    }
    val classifier = mkClassifier
    classifier.train(dataset)
    logger.debug("Finished training the first pass model for this fold.")
    classifier
  }

  private def mkDataset: Dataset[L, F] = new RVFDataset[L, F]()
  private def mkDatum(label:L, features:Counter[F]): Datum[L, F] = new RVFDatum[L, F](label, features)
  private def mkClassifier: Classifier[L, F] = new L1LogisticRegressionClassifier[L, F]()

  override def classesOf(sentence: Sentence):List[L] = {
    val firstPassLabels = predict(firstPassModel.get, sentence, None, leftToRightFirstPass)
    predict(secondPassModel.get, sentence, Some(firstPassLabels), leftToRightSecondPass)
  }

  private def predict(classifier:Classifier[L, F],
                      origSentence: Sentence,
                      firstPassLabels:Option[Seq[L]],
                      leftToRight:Boolean): List[L] = {
    val sent = if(leftToRight) origSentence else origSentence.revert()
    val fpls = firstPassLabels // TODO: change to Array here

    val history = new ArrayBuffer[L]()
    for(i <- 0 until sent.size) {
      val feats = new Counter[F]
      featureExtractor(feats, sent, i)
      addHistoryFeatures(feats, order, history, i)
      if(fpls.nonEmpty) {
        addFirstPassFeatures(feats, order, fpls.get, i)
      }
      val d = mkDatum(null.asInstanceOf[L], feats)
      val label = classifier.classOf(d)
      history += label
    }
    history.toList
  }

  override def save(fn:File): Unit = {
    var w = new PrintWriter(new FileWriter(fn + ".first"))
    w.println(order)
    firstPassModel.get.saveTo(w)
    w.close()
    w = new PrintWriter(new FileWriter(fn + ".second"))
    secondPassModel.get.saveTo(w)
    w.close()
  }

  override def load(is:InputStream) {
    val reader = new BufferedReader(new InputStreamReader(is))
    order = reader.readLine().toInt
    val fpc = LiblinearClassifier.loadFrom[L, F] (reader)
    firstPassModel = Some(fpc)
    val spc = LiblinearClassifier.loadFrom[L, F] (reader)
    secondPassModel = Some(spc)
  }
}
