package org.clulab.processors.clu.sequences

import java.io._

import org.clulab.learning._
import org.clulab.processors.{Document, Sentence}

import scala.collection.mutable.ArrayBuffer
import SequenceTaggerLogger._
import org.clulab.struct.Counter
import org.clulab.utils.SeqUtils

import scala.reflect.ClassTag

/**
  * Bidirectional MEMM sequence tagger
  * User: mihais
  * Date: 8/27/17
  */
abstract class BiMEMMSequenceTagger[L: ClassTag, F](
  var order:Int = 1,
  val numFoldsFirstPass:Int = 5,
  val leftToRightSecondPass:Boolean = true) extends SequenceTagger[L, F] {
  
  var firstPassModel:Option[Classifier[L, F]] = None
  var secondPassModel:Option[Classifier[L, F]] = None

  override def train(docs:Iterator[Document]): Unit = {
    val sentences = new ArrayBuffer[Sentence]()
    for(doc <- docs; sent <- doc.sentences) {
      sentences += sent
    }
    logger.info(s"Training on ${sentences.size} sentences using order $order.")

    // generate first-pass labels
    val firstPassLabels = mkFirstPassLabels(sentences)
    assert(firstPassLabels.length == sentences.size)

    // make and save the first-pass classifier on the whole data
    firstPassModel = Some(buildClassifier(sentences, mkFullFold(sentences.size),
      ! leftToRightSecondPass, None))

    // make the second-pass classifier
    secondPassModel = Some(buildClassifier(sentences, mkFullFold(sentences.size),
      leftToRightSecondPass, Some(firstPassLabels)))
  }

  def mkFirstPassLabels(sentences: ArrayBuffer[Sentence]): Array[Array[L]] = {
    val folds = Datasets.mkFolds(numFoldsFirstPass, sentences.size)

    // generate first-pass labels through cross validation
    logger.debug("Generating first pass labels...")
    val labels = new Array[Array[L]](sentences.size)
    var foldCount = 1
    for(fold <- folds) {
      logger.debug(s"In fold $foldCount: ${fold.testFold}...")
      foldCount += 1

      val classifier = buildClassifier(sentences, fold, ! leftToRightSecondPass, None)
      for(si <- fold.testFold._1 until fold.testFold._2) {
        labels(si) = classesOf(classifier, sentences(si), None, ! leftToRightSecondPass)
      }
    }

    // check the accuracy of these labels
    var total = 0
    var correct = 0
    for(i <- sentences.indices) {
      val sent = sentences(i)
      val gold = labelExtractor(sent)
      val pred = labels(i)
      assert(gold != null && pred != null)
      assert(gold.length == pred.length)
      total += gold.length
      for(j <- gold.indices) {
        if(gold(j) == pred(j)) correct += 1
      }
    }
    logger.info(s"Accuracy of first pass classifier: ${100.0 * correct.toDouble / total.toDouble}% ($correct/$total)")

    labels
  }

  def buildClassifier(
    sentences: ArrayBuffer[Sentence],
    fold: DatasetFold,
    leftToRight:Boolean,
    firstPassLabels:Option[Array[Array[L]]]): Classifier[L, F] = {

    // construct the dataset from the training partitions
    val dataset = mkDataset
    var sentCount = 0
    for (trainFold <- fold.trainFolds; sentOffset <- trainFold._1 until trainFold._2) {
      val origSentence = sentences(sentOffset)
      val sentence = if (leftToRight) origSentence else origSentence.revert()
      val labels =
        if (leftToRight) labelExtractor(origSentence)
        else SeqUtils.revert(labelExtractor(origSentence)).toArray

      //
      // add features from observed data
      //
      val features = new Array[Counter[F]](sentence.size)
      for (i <- features.indices) features(i) = new Counter[F]()
      for(i <- 0 until sentence.size)
        featureExtractor(features(i), sentence, i)

      //
      // add history features:
      //   concatenate the labels of the previous <order> tokens to the features
      // then store each example in the training dataset
      //
      for(i <- features.indices)
        addHistoryFeatures(features(i), order, labels, i)

      //
      // add features from first-pass labels (if any)
      //
      if(firstPassLabels.nonEmpty) {
        val firstPass =
          if (leftToRight) firstPassLabels.get(sentOffset)
          else SeqUtils.revert(firstPassLabels.get(sentOffset)).toArray

        for(i <- features.indices)
          addFirstPassFeatures(features(i), order, firstPass, i)
      }

      // add one datum for each word in the sentence
      assert(labels.length == features.length)
      for(i <- labels.indices) {
        assert(labels(i) != null)
        assert(features(i) != null)
        val d = mkDatum(labels(i), features(i))
        dataset += d
      }

      sentCount += 1
      if (sentCount % 100 == 0) {
        logger.debug(s"Processed $sentCount sentences...")
      }
    }

    // train
    val classifier = mkClassifier
    classifier.train(dataset)

    classifier
  }

  def classesOf(classifier: Classifier[L, F],
                origSentence: Sentence,
                firstPassLabels:Option[Array[L]],
                leftToRight:Boolean): Array[L] = {
    val sentence = if(leftToRight) origSentence else origSentence.revert()

    val history = new ArrayBuffer[L]()
    for(i <- 0 until sentence.size) {
      val feats = new Counter[F]
      featureExtractor(feats, sentence, i)
      addHistoryFeatures(feats, order, history, i)
      val d = mkDatum(null.asInstanceOf[L], feats)
      val label = classifier.classOf(d)
      history += label
    }

    if(leftToRight) history.toArray else SeqUtils.revert(history).toArray
  }

  override def classesOf(sentence: Sentence):Array[L] = {
    val firstPassLabels = classesOf(firstPassModel.get, sentence, None, ! leftToRightSecondPass)
    val secondPassLabels = classesOf(secondPassModel.get, sentence, Some(firstPassLabels), leftToRightSecondPass)
    secondPassLabels
  }

  private def mkDataset: Dataset[L, F] = new RVFDataset[L, F]()
  private def mkDatum(label:L, features:Counter[F]): Datum[L, F] = new RVFDatum[L, F](label, features)
  private def mkClassifier: Classifier[L, F] = new L1LogisticRegressionClassifier[L, F]()
  private def mkFullFold(size:Int): DatasetFold =
    new DatasetFold(testFold = Tuple2(-1, -1), trainFolds = List(Tuple2(0, size)))

  override def save(fn:File): Unit = {

    // save order + first pass model
    var w = new PrintWriter(new FileWriter(fn))
    w.println(order)
    firstPassModel.get.saveTo(w)
    w.close()

    w = new PrintWriter(new FileWriter(fn, true))
    secondPassModel.get.saveTo(w)
    w.close()

    /*
    // save second pass model in a separate file // TODO: this is not saved in the PrintWriter above; why?
    val secPassFile = new File(fn + ".second.tmp")
    w = new PrintWriter(new FileWriter(secPassFile))
    secondPassModel.get.saveTo(w)
    w.close()

    // append second pass model to main file
    w = new PrintWriter(new FileWriter(fn, true))
    val source = io.Source.fromFile(secPassFile)
    for(line <- source.getLines())
      w.println(line)
    source.close()
    w.close()

    secPassFile.delete()
    */
  }

  override def load(is:InputStream) {
    val reader = new BufferedReader(new InputStreamReader(is))
    order = reader.readLine().toInt
    val fpc = LiblinearClassifier.loadFrom[L, F] (reader)
    firstPassModel = Some(fpc)
    reader.readLine()
    val spc = LiblinearClassifier.loadFrom[L, F] (reader)
    secondPassModel = Some(spc)
  }
}
