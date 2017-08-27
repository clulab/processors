package org.clulab.processors.clu.sequences

import java.io._

import org.clulab.learning._
import org.clulab.processors.{Document, Sentence}
import SequenceTaggerLogger._

import scala.collection.mutable.ArrayBuffer

/**
  * Sequence tagger using a maximum entrop Markov model (MEMM)
  * User: mihais
  * Date: 8/26/17
  */
abstract class MEMMSequenceTagger[L, F](var order:Int = 1) extends SequenceTagger[L, F] {
  var model:Option[Classifier[L, F]] = None

  override def train(docs:Iterator[Document]): Unit = {
    val dataset = new BVFDataset[L, F]()

    logger.debug(s"Generating features using order $order...")
    var sentCount = 0
    for(doc <- docs; sentence <- doc.sentences) {
      // labels and features for one sentence
      val labels = labelExtractor(sentence)
      val features = (0 until sentence.size).map(featureExtractor(sentence, _)).toArray

      //
      // add history features:
      //   concatenate the labels of the previous <order> tokens to the features
      // then store each example in the training dataset
      //
      for(i <- features.indices) {
        features(i) = addHistoryFeatures(features(i), order, labels, i)
        val d = new BVFDatum[L, F](labels(i), features(i))
        dataset += d
      }

      sentCount += 1
      if(sentCount % 100 == 0) {
        logger.debug(s"Processed $sentCount sentences...")
      }
    }
    logger.debug("Finished processing all sentences.")

    val classifier = new L1LogisticRegressionClassifier[L, F]()
    logger.debug("Started training the classifier...")
    classifier.train(dataset)
    model = Some(classifier)
    logger.debug("Finished training.")
  }

  override def classesOf(sentence: Sentence):List[L] = {
    val history = new ArrayBuffer[L]()
    for(i <- 0 until sentence.size) {
      val feats = addHistoryFeatures(featureExtractor(sentence, i), order, history, i)
      val d = new BVFDatum[L, F](null.asInstanceOf[L], feats)
      val label = model.get.classOf(d)
      history += label
    }
    history.toList
  }

  override def save(fn:File): Unit = {
    val w = new PrintWriter(new FileWriter(fn))
    w.println(order)
    model.get.saveTo(w)
    w.close()
  }

  override def load(is:InputStream) {
    val reader = new BufferedReader(new InputStreamReader(is))
    order = reader.readLine().toInt
    val c = LiblinearClassifier.loadFrom[L, F] (reader)
    model = Some(c)
  }
  
}

