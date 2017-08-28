package org.clulab.processors.clu.sequences

import java.io._

import org.clulab.learning._
import org.clulab.processors.{Document, Sentence}

import scala.collection.mutable.ArrayBuffer
import SequenceTaggerLogger._

/**
  * Bidirectional MEMM sequence tagger
  * User: mihais
  * Date: 8/27/17
  */
abstract class BiMEMMSequenceTagger[L, F](var order:Int = 1) extends SequenceTagger[L, F] {
  var firstPassModel:Option[Classifier[L, F]] = None
  var secondPassModel:Option[Classifier[L, F]] = None
  val numFolds = 3

  override def train(docs:Iterator[Document]): Unit = {
    val sentences = new ArrayBuffer[Sentence]()
    for(doc <- docs; sent <- doc.sentences) {
      sentences += sent
    }

    logger.debug(s"Training on ${sentences.size} sentences.")
    logger.debug(s"Generating features using order $order...")
    val folds = Datasets.mkFolds(numFolds, sentences.size)
    logger.debug(s"Using $numFolds folds for the first pass model.")

    // create dataset for the second pass model
    // this creates a different first pass model for each fold
    val dataset = mkDataset
    var foldCount = 1
    for(fold <- folds) {
      logger.debug(s"In fold $foldCount: ${fold.testFold}...")
      foldCount += 1

      val firstPass = mkFirstPassModelOnFold(fold, sentences)
      addToDataset(dataset, sentences, fold.testFold, Some(firstPass), leftToRight = true)
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
    addToDataset(firstPassDataset, sentences, Tuple2(0, sentences.size), None, leftToRight = false)
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
      val sentence = sentences(sentOffset)
      
      // labels and features for one sentence
      val labels = labelExtractor(sentence)
      val features = (0 until sentence.size).map(featureExtractor(sentence, _)).toArray

      // add history features: concatenate the labels of the previous <order> tokens to the features
      for(i <- features.indices) {
        // TODO: adapt for r-to-l
        features(i) = addHistoryFeatures(features(i), order, labels, i)
      }

      // add first pass features: the label predicted by the first pass model
      if(firstPass.nonEmpty) {
        // TODO: get labels from first pass, add label(i) as feature
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
      addToDataset(dataset, sentences, tf, None, leftToRight = false)
    }
    val classifier = mkClassifier
    classifier.train(dataset)
    logger.debug("Finished training the first pass model for this fold.")
    classifier
  }

  private def mkDataset: Dataset[L, F] = new BVFDataset[L, F]()
  private def mkDatum(label:L, features:Iterable[F]): Datum[L, F] = new BVFDatum[L, F](label, features)
  private def mkClassifier: Classifier[L, F] = new L1LogisticRegressionClassifier[L, F]()

  override def classesOf(sentence: Sentence):List[L] = {
    null // TODO
  }

  override def save(fn:File): Unit = {
    val w = new PrintWriter(new FileWriter(fn))
    w.println(order)
    firstPassModel.get.saveTo(w)
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
