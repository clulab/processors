package org.clulab.sequences

import java.io._

import org.clulab.learning._
import org.clulab.processors.{Document, Sentence}
import org.clulab.sequences.SequenceTaggerLogger._
import org.clulab.struct.Counter
import org.clulab.utils.SeqUtils

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
  * Bidirectional MEMM sequence tagger
  * User: mihais
  * Date: 8/27/17
  */
abstract class BiMEMMSequenceTagger[L: ClassTag, F](
  var order:Int,
  var numFoldsFirstPass:Int, // if < 2, this reverts to a single-layer MEMM
  var leftToRight:Boolean) extends SequenceTagger[L, F] {

  /** C'tor for a single-layer. left-to-right MEMM of order 2 */
  def this(order:Int = 2, leftToRight:Boolean = true) = {
    this(order, -1, leftToRight)
  }

  var firstPassModel:Option[Classifier[L, F]] = None
  var secondPassModel:Option[Classifier[L, F]] = None

  override def train(docs:Iterator[Document]): Unit = {
    val sentences = new ArrayBuffer[Sentence]()
    for(doc <- docs; sent <- doc.sentences) {
      sentences += sent
    }
    logger.info(s"Training on ${sentences.size} sentences using order $order.")

    // count bigrams in the corpus, and keep a set of those that occur > BIGRAM_THRESHOLD times
    FeatureExtractor.countBigrams(sentences, FeatureExtractor.BIGRAM_THRESHOLD)

    var firstPassLabels:Option[Array[Array[L]]] = None
    var acc = 0.0
    if(numFoldsFirstPass > 1) {
      // generate first-pass labels
      // try to read them from cached file, if it exists
      val firstPassFile = new File(FIRST_PASS_FILE)
      firstPassLabels = if(firstPassFile.exists()) {
        logger.debug(s"Found cached file with first-pass labels: $FIRST_PASS_FILE")
        val source = scala.io.Source.fromFile(firstPassFile)
        val labels = readFirstPassLabels(source)
        source.close()
        Some(labels)
      } else {
        logger.debug("Generating first-pass labels from scratch...")
        val labels = mkFirstPassLabels(sentences)
        val pw = new PrintWriter(new FileWriter(FIRST_PASS_FILE))
        for(s <- labels) {
          pw.println(s.mkString("\t"))
        }
        pw.close()
        Some(labels)
      }
      assert(firstPassLabels.get.length >= sentences.size)

      // compute the accuracy of the first pass
      acc = accuracy(sentences, firstPassLabels.get)

    }

    // make the second-pass classifier
    logger.debug("Training the second-pass classifier on the whole data...")
    secondPassModel = Some(buildClassifier(sentences, mkFullFold(sentences.size),
      leftToRight, firstPassLabels))

    if(numFoldsFirstPass > 1) {
      // make the first-pass classifier on the whole data
      logger.debug("Training the first-pass classifier on the whole data...")
      firstPassModel = Some(buildClassifier(sentences, mkFullFold(sentences.size),
        !leftToRight, None))
    }

    logger.info("Finished training.")
    if(firstPassLabels.nonEmpty)
      logger.info(s"The accuracy of the first pass classifier was $acc.")
  }

  private val FIRST_PASS_FILE = "first_pass_labels.tsv"
  protected def readFirstPassLabels(source:scala.io.Source):Array[Array[L]]

  def mkFirstPassLabels(sentences: ArrayBuffer[Sentence]): Array[Array[L]] = {
    val folds = Datasets.mkFolds(numFoldsFirstPass, sentences.size)

    // generate first-pass labels through cross validation
    logger.debug("Generating first pass labels...")
    val labels = new Array[Array[L]](sentences.size)
    var foldCount = 1
    for(fold <- folds) {
      logger.debug(s"In fold $foldCount: ${fold.testFold}...")
      foldCount += 1

      val classifier = buildClassifier(sentences, fold, ! leftToRight, None)
      for(si <- fold.testFold._1 until fold.testFold._2) {
        labels(si) = classesOf(classifier, sentences(si), None, ! leftToRight)
      }
    }

    labels
  }

  def accuracy(sentences: ArrayBuffer[Sentence], labels:Array[Array[L]]):Double = {
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
    val acc = 100.0 * correct.toDouble / total.toDouble
    logger.info(s"Accuracy of first pass classifier: $acc% ($correct/$total)")
    acc
  }

  def mkFeatures(features:Counter[F],
                 sentence:Sentence,
                 offset:Int,
                 history:Seq[L],
                 firstPassLabels:Option[Array[L]]): Unit = {
    //
    // add features from observed data
    //
    featureExtractor(features, sentence, offset)

    //
    // add history features:
    //   concatenate the labels of the previous <order> tokens to the features
    // then store each example in the training dataset
    //
    addHistoryFeatures(features, order, history, offset)

    //
    // add features from first-pass labels (if any)
    //
    if (firstPassLabels.nonEmpty) {
      addFirstPassFeatures(features, order, firstPassLabels.get, offset)
    }
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
      // original sentence
      val origSentence = sentences(sentOffset)
      // actual sentence to be used
      val sentence = if (leftToRight) origSentence else origSentence.revert()
      // labels to be learned
      val labels =
        if (leftToRight) labelExtractor(origSentence)
        else SeqUtils.revert(labelExtractor(origSentence)).toArray
      // labels from the first pass (if any)
      val firstPass =
        if(firstPassLabels.nonEmpty) {
          if(leftToRight) Some(firstPassLabels.get(sentOffset))
          else Some(SeqUtils.revert(firstPassLabels.get(sentOffset)).toArray)
        } else {
          None
        }

      val features = new Array[Counter[F]](sentence.size)
      assert(labels.length == features.length)
      for (i <- features.indices) features(i) = new Counter[F]()
      for(i <- 0 until sentence.size) {
        // add all features
        mkFeatures(features(i), sentence, i, labels, firstPass)

        // add one datum for each word in the sentence
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

    val firstPass =
      if(firstPassLabels.nonEmpty) {
        if(leftToRight) firstPassLabels
        else Some(SeqUtils.revert(firstPassLabels.get).toArray)
      } else {
        None
      }

    val history = new ArrayBuffer[L]()
    for(i <- 0 until sentence.size) {
      val feats = new Counter[F]
      mkFeatures(feats, sentence, i, history, firstPass)
      val d = mkDatum(null.asInstanceOf[L], feats)
      val label = classifier.classOf(d)
      history += label
    }

    if(leftToRight) history.toArray else SeqUtils.revert(history).toArray
  }

  override def classesOf(sentence: Sentence):Array[L] = {
    var firstPassLabels:Option[Array[L]] = None
    if(firstPassModel.nonEmpty)
      firstPassLabels = Some(classesOf(firstPassModel.get, sentence, None, ! leftToRight))
    val secondPassLabels = classesOf(secondPassModel.get, sentence, firstPassLabels, leftToRight)
    secondPassLabels
  }

  private def mkDataset: Dataset[L, F] = new RVFDataset[L, F]()
  private def mkDatum(label:L, features:Counter[F]): Datum[L, F] = new RVFDatum[L, F](label, features)
  private def mkClassifier: Classifier[L, F] = new L1LogisticRegressionClassifier[L, F]() // TODO: add all classifiers
  private def mkFullFold(size:Int): DatasetFold =
    new DatasetFold(testFold = Tuple2(-1, -1), trainFolds = List(Tuple2(0, size)))

  override def save(fn:File): Unit = {
    // save meta data
    var w = new PrintWriter(new FileWriter(fn))
    w.println(order)
    w.println(leftToRight)

    // save second pass model
    secondPassModel.get.saveTo(w)
    w.close()

    // save first pass model (if any)
    w = new PrintWriter(new FileWriter(fn, true))
    if(firstPassModel.nonEmpty) {
      w.println(1)
      firstPassModel.get.saveTo(w)
    } else {
      w.println(0)
    }
    w.close()
  }

  override def load(reader:BufferedReader): Unit = {
    // load meta data
    order = reader.readLine().toInt
    leftToRight = reader.readLine().toBoolean

    // load second pass classifier
    secondPassModel = Some(LiblinearClassifier.loadFrom[L, F] (reader))
    reader.readLine()

    // load first pass classifier (if any)
    val hasFirstPass = reader.readLine().toInt
    if(hasFirstPass == 1) {
      firstPassModel = Some(LiblinearClassifier.loadFrom[L, F](reader))
    } else {
      firstPassModel = None
    }
    reader.close()
  }
}
