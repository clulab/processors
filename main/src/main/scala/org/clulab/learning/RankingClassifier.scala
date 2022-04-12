package org.clulab.learning

import org.clulab.utils.MathUtils._
import java.util.Properties
import collection.mutable.{ListBuffer, ArrayBuffer}
import org.clulab.struct.Counter
import java.io.PrintWriter

/**
 * Generic trait for ranking classifiers; for iid classification see Classifier
 * User: mihais
 * Date: 4/23/13
 */
trait RankingClassifier[F] {
  def train(dataset:RankingDataset[F], spans:Option[Iterable[(Int, Int)]] = None): Unit

  /** Displays the learned model in a human-readable format, for debug purposes */
  def displayModel(pw:PrintWriter): Unit

  /**
   * Returns scores that can be used for ranking for a group of datums, from the same query
   * These scores do NOT have to be normalized, they are NOT probabilities!
   * @param queryDatums All datums for one query
   * @return
   */
  def scoresOf(queryDatums:Iterable[Datum[Int, F]]):Iterable[Double]

  /**
   * Returns probabilities that can be used for ranking for a group of datums, from the same query
   * These probabilities are obtained here from scoresOf() using softmax
   * @param queryDatums All datums for one query
   * @return
   */
  def probabilitiesOf(queryDatums:Iterable[Datum[Int, F]], gamma:Double = 1.0):Iterable[Double] = {
    val scores = scoresOf(queryDatums)
    softmax(scores, gamma)
  }

  /** Saves the current model to a file */
  def saveTo(fileName:String): Unit
}

object RankingClassifier {
  /**
   * Generate scores on this dataset using cross validation
   * @param dataset The dataset
   * @param numFolds Number of folds for cross validation
   * @return
   */
  def crossValidate[F](
                        dataset:RankingDataset[F],
                        classifierProperties:Properties,
                        numFolds:Int = 10,
                        generateProbabilities:Boolean = false,
                        softmaxGamma:Double = 1.0):Array[Array[Double]] = {

    val folds = Datasets.mkFolds(numFolds, dataset.size)
    val scores = new Array[Array[Double]](dataset.size)

    var foldOffset = 1
    for(fold <- folds) {
      val props = new Properties(classifierProperties)
      var debugFile = props.getProperty("debugFile")
      if(debugFile != null && debugFile.length > 0) {
        debugFile = debugFile + "." + foldOffset
        props.setProperty("debugFile", debugFile)
      }
      val classifier:RankingClassifier[F] = apply(props)
      classifier.train(dataset, Some(fold.trainFolds))

      for(i <- fold.testFold._1 until fold.testFold._2) {
        val queryDatums = dataset.mkQueryDatums(i)
        if(generateProbabilities) {
          scores(i) = classifier.probabilitiesOf(queryDatums, softmaxGamma).toArray
        } else {
          scores(i) = classifier.scoresOf(queryDatums).toArray
        }
      }
      foldOffset += 1
    }

    scores
  }

  /**
   * Factory method for RankingClassifier
   * Creates a ranking classifier of the type given in the "classifierClass" property
   * @param properties
   * @tparam F
   * @return
   */
  def apply[F](properties:Properties):RankingClassifier[F] = {
    if(! properties.containsKey("classifierClass")) {
      return new SVMRankingClassifier[F](properties)
      //return new PerceptronRankingClassifier[F](properties)
    }

    properties.getProperty("classifierClass") match {
      case "SVMRankingClassifier" => new SVMRankingClassifier[F](properties)
      case "PerceptronRankingClassifier" => new PerceptronRankingClassifier[F](properties)
      case _ => throw new RuntimeException("ERROR: unknown ranking classifier type: " +
        properties.getProperty("classifierType") + "!")
    }
  }
}
