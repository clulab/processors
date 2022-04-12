package org.clulab.learning

import java.io.{Reader, Writer}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import org.clulab.struct.Counter
import org.clulab.utils.ThreadUtils

import scala.collection.mutable
import org.slf4j.{Logger, LoggerFactory}

/**
  * Operations on datasets
  * User: mihais
  * Date: 5/1/13
  * Last Modified: Update for Scala 2.12: fork join changes.
  */
class Datasets

object Datasets {

  val logger: Logger = LoggerFactory.getLogger(classOf[Datasets])

  /** Creates dataset folds to be used for cross validation */
  def mkFolds(numFolds:Int, size:Int):Iterable[DatasetFold] = {
    val foldSize:Int = size / numFolds
    val folds = new ArrayBuffer[DatasetFold]
    for(i <- 0 until numFolds) {
      val startTest = i * foldSize
      var endTest = (i + 1) * foldSize
      if(i == numFolds - 1)
        endTest = math.max(size, endTest)

      val trainFolds = new ArrayBuffer[(Int, Int)]
      if(startTest > 0)
        trainFolds += Tuple2(0, startTest)
      if(endTest < size)
        trainFolds += Tuple2(endTest, size)

      folds += new DatasetFold(Tuple2(startTest, endTest), trainFolds.toList)
    }
    folds.toList
  }

  def mkTrainIndices[F](datasetSize:Int, spans:Option[Iterable[(Int, Int)]]):Array[Int] = {
    val indices = new ArrayBuffer[Int]()
    val trainFolds = spans.getOrElse(mkFullFold(datasetSize))
    for(fold <- trainFolds) {
      for(i <- fold._1 until fold._2) {
        indices += i
      }
    }
    indices.toArray
  }

  private def mkFullFold(size:Int): Iterable[(Int, Int)] = {
    val folds = new Array[(Int, Int)](1)
    folds(0) = Tuple2(0, size)
    folds
  }

  /** Scales feature values using the svm-scale formula. Scaling is performed in place */
  def svmScaleDataset[L, F](dataset:Dataset[L, F], lower:Double = -1, upper:Double = 1):ScaleRange[F] = {
    dataset match {
      case rvf:RVFDataset[L, F] =>
        svmScaleRVFDataset(dataset.asInstanceOf[RVFDataset[L, F]], lower, upper)
      case bvf:BVFDataset[L, F] =>
        svmScaleBVFDataset(dataset.asInstanceOf[BVFDataset[L, F]], lower, upper)
      case _ =>
        throw new RuntimeException("ERROR: unknown dataset type in svmScale!")
    }
  }

  /** The same functionality as svmScaleDataset, but applied to an individual datum */
  def svmScaleDatum[F](features:Counter[F], ranges:ScaleRange[F], lower:Double = -1, upper:Double = 1):Counter[F] = {
    assert(ranges != null)
    assert(features != null)
    val scaledFeatures = new Counter[F]()
    for(f <- features.keySet) {
      val v = features.getCount(f)
      var min:Double = 0.0
      var max:Double = 0.0
      if(ranges.contains(f)) {
        min = ranges.min(f)
        max = ranges.max(f)
      }
      scaledFeatures.setCount(f, scale(v, min, max, lower, upper))
    }
    scaledFeatures
  }

  /** Scales feature values using the svm-scale formula. Scaling is performed in place */
  def svmScaleRankingDataset[L, F](dataset:RankingDataset[F], lower:Double = -1, upper:Double = 1): ScaleRange[F] = {
    try {
      svmScaleFeatureTraversable(dataset.asInstanceOf[FeatureTraversable[F, Double]], lower, upper)
    } catch  {
      case e:ClassCastException => throw new RuntimeException("Feature traverser not implemented! " + e.getMessage)
    }
  }

  def svmScaleFeatureTraversable[F](dataset: FeatureTraversable[F, Double], lower: Double, upper: Double): ScaleRange[F] = {
    val ranges = new ScaleRange[F]
    // scan the dataset once and keep track of min/max for each feature
    dataset.featureUpdater.foreach {
      case (f, v) => ranges.update(f, v)
    }
    // scan again and update
    dataset.featureUpdater.updateAll {
      case (f, v) => scale(v, ranges.min(f), ranges.max(f), lower, upper)
    }
    ranges
  }
  def svmScaleBVFDataset[L, F](dataset:BVFDataset[L, F], lower:Double, upper:Double):ScaleRange[F] = {
    throw new RuntimeException("ERROR: scaling of BVF datasets not implemented yet!")
  }

  def svmScaleRVFDataset[L, F](dataset:RVFDataset[L, F], lower:Double, upper:Double):ScaleRange[F] = {
    // scan the dataset once and keep track of min/max for each feature
    val ranges = new ScaleRange[F]
    for(i <- 0 until dataset.size) {
      for(j <- dataset.features(i).indices) {
        val fi = dataset.features(i)(j)
        val v = dataset.values(i)(j)
        val f = dataset.featureLexicon.get(fi)
        ranges.update(f, v)
      }
    }

    // traverse the dataset again and scale values for all features
    for(i <- 0 until dataset.size) {
      for (j <- dataset.features(i).indices) {
        val fi = dataset.features(i)(j)
        val v = dataset.values(i)(j)
        val f = dataset.featureLexicon.get(fi)
        dataset.values(i)(j) = scale(v, ranges.min(f), ranges.max(f), lower, upper)
      }
    }

    ranges
  }

  /** The actual scaling formula taken from svm-scale */
  private def scale(value:Double, min:Double, max:Double, lower:Double, upper:Double):Double = {
    if(min == max) return upper

    // the result will be a value in [lower, upper]
    lower + (upper - lower) * (value - min) / (max - min)
  }

  /**
   * Performs incremental feature selection through cross-validation on the given dataset
   */
  def incrementalFeatureSelection[L, F](
    dataset:Dataset[L, F],
    classifierFactory: () => Classifier[L, F],
    scoringMetric: (Iterable[(L, L)]) => Double,
    featureGroups:Map[String, Set[Int]],
    numFolds:Int = 5,
    nCores:Int = 8):Set[String] = {

    // first, let's find the performance using all features
    val datasetOutput = crossValidate(dataset, classifierFactory, numFolds)
    val datasetScore = scoringMetric(datasetOutput)
    logger.info(s"Iteration #0: Score using ALL features is $datasetScore.")
    logger.info(s"Iteration #0: Using ${featureGroups.size} feature groups and ${dataset.featureLexicon.size} features.")

    val chosenGroups = new mutable.HashSet[String]()
    val allBetterChosenGroups = new mutable.HashSet[String]()
    val chosenFeatures = new mutable.HashSet[Int]()
    var bestScore = Double.MinValue
    var iteration = 1
    var meatLeftOnTheBone = true
    while(meatLeftOnTheBone) {
      var bestGroup:String = null
      var bestFeatures:Set[Int] = null
      val workingGroups = ThreadUtils.parallelize(featureGroups.keySet.filter(! chosenGroups.contains(_)), nCores)

      // this is parallelized!
      val scores = workingGroups.map(scoreGroup(_,
        featureGroups, chosenFeatures, dataset, classifierFactory, numFolds, scoringMetric)).toList

      for (gs <- scores) {
        val group: String = gs._1
        val score = gs._2

        if(score > bestScore) {
          bestScore = score
          bestGroup = group
          bestFeatures = featureGroups(group)
          logger.debug(s"Iteration #$iteration: found new best group [$bestGroup] with score $bestScore.")
          if(iteration > 1) allBetterChosenGroups += bestGroup
        }
      }


      if(bestGroup == null) {
        meatLeftOnTheBone = false
        logger.info(s"Iteration #$iteration: no better group found. Search complete.")
      } else {
        logger.info(s"Iteration #$iteration: best group found is [$bestGroup] with score $bestScore.")
        chosenGroups += bestGroup
        chosenFeatures ++= bestFeatures
        logger.info(s"Iteration #$iteration: we now have ${chosenGroups.size} chosen groups and ${chosenFeatures.size} chosen features.")
      }

      iteration += 1
    }

    logger.info(s"Iteration #$iteration: process ended with score $bestScore using ${chosenGroups.size} chosen groups and ${chosenFeatures.size} chosen features.")
    logger.info(s"Found ${allBetterChosenGroups.size} better groups: ${allBetterChosenGroups.toSet}")
    chosenGroups.toSet
  }

  def featureSelectionByInformativeness[L, F](
        dataset:Dataset[L, F],
        classifierFactory: () => Classifier[L, F],
        scoringMetric: (Iterable[(L, L)]) => Double,
        minFreq:Int = 10,
        numFolds:Int = 5,
        step:Int = 1000):Set[Int] = {
    // first, let's find the performance using all features
    val datasetOutput = crossValidate(dataset, classifierFactory, numFolds)
    val datasetScore = scoringMetric(datasetOutput)
    logger.info(s"Score using ALL features is $datasetScore.")

    val features = Datasets.sortFeaturesByInformativeness(dataset, minFreq).sorted.toArray
    logger.debug("Top 20 most informative features:")
    for(i <- 0 until math.min(20, features.length)) {
      logger.debug(dataset.featureLexicon.get(features(i)._1) + "\t" + features(i)._2)
    }

    var bestScore = Double.MinValue
    var bestCut = 0

    var cut = math.min(step, features.length)
    var meatLeftOnTheBone = true
    while(cut <= features.length && meatLeftOnTheBone) {
      val smallDataset = dataset.keepOnly(features.slice(0, cut).map(_._1).toSet)
      val output = crossValidate(smallDataset, classifierFactory, numFolds)
      val score = scoringMetric(output)

      if(score > bestScore) {
        bestScore = score
        bestCut = cut
        logger.debug(s"Found better cut at $bestCut with score $bestScore")
      } else {
        meatLeftOnTheBone = false
      }

      cut = math.min(features.length, cut + step)
    }

    logger.info(s"Cutting features at $bestCut out of ${features.length}.")
    features.slice(0, bestCut).map(_._1).toSet
  }

  def featureSelectionByFrequency[L, F](
                                        dataset:Dataset[L, F],
                                        classifierFactory: () => Classifier[L, F],
                                        scoringMetric: (Iterable[(L, L)]) => Double,
                                        numFolds:Int = 5):Set[Int] = {

    // first, let's find the performance using all features
    val datasetOutput = crossValidate(dataset, classifierFactory, numFolds)
    val datasetScore = scoringMetric(datasetOutput)
    logger.info(s"Score using ALL features is $datasetScore.")

    val features = Datasets.sortFeaturesByFrequency(dataset)
    var bestScore = datasetScore
    var bestCut = 0

    var meatLeftOnTheBone = true
    for(t <- 1 until 100 if meatLeftOnTheBone) {
      val smallFeats = keepMoreFrequent(features, t)
      if(smallFeats.isEmpty) {
        meatLeftOnTheBone = false
      } else {
        val smallDataset = dataset.keepOnly(smallFeats)
        val output = crossValidate(smallDataset, classifierFactory, numFolds)
        val score = scoringMetric(output)

        if (score > bestScore) {
          bestScore = score
          bestCut = t
          logger.debug(s"Found better frequency cutoff at $bestCut with score $bestScore")
        } else {
          meatLeftOnTheBone = false
        }
      }
    }

    logger.info(s"Cutting features at $bestCut out of ${features.size}.")
    keepMoreFrequent(features, bestCut)
  }

  def keepMoreFrequent(features:Counter[Int], threshold:Double):Set[Int] = {
    val s = new mutable.HashSet[Int]()
    for(f <- features.keySet) {
      if(features.getCount(f) > threshold)
        s += f
    }
    s.toSet
  }

  def scoreGroup[L, F](group:String,
                       featureGroups:Map[String, Set[Int]],
                       chosenFeatures:mutable.HashSet[Int],
                       dataset:Dataset[L, F],
                       classifierFactory: () => Classifier[L, F],
                       numFolds:Int,
                       scoringMetric: (Iterable[(L, L)]) => Double):(String, Double) = {
    val currentFeatures = new mutable.HashSet[Int]()
    currentFeatures ++= chosenFeatures
    currentFeatures ++= featureGroups(group)

    val score = scoreFeatures(dataset, currentFeatures, classifierFactory, numFolds, scoringMetric)

    (group, score)
  }

  def scoreFeatures[L, F](dataset:Dataset[L, F],
                          features:mutable.HashSet[Int],
                          classifierFactory: () => Classifier[L, F],
                          numFolds:Int,
                          scoringMetric: (Iterable[(L, L)]) => Double):Double = {
    val filteredDataset = dataset.keepOnly(features.toSet)
    val output = Datasets.crossValidate(filteredDataset, classifierFactory, numFolds)
    val score = scoringMetric(output)
    score
  }

  /**
   * Implements classic cross validation; producing pairs of gold/predicted labels across the training dataset
   */
  def crossValidate[L, F](
    dataset:Dataset[L, F],
    classifierFactory: () => Classifier[L, F],
    numFolds:Int = 5):Iterable[(L, L)] = {

    val folds = Datasets.mkFolds(numFolds, dataset.size)
    val output = new ListBuffer[(L, L)]

    for(fold <- folds) {
      val classifier = classifierFactory()
      classifier.train(dataset, Some(fold.trainFolds))
      for(i <- fold.testFold._1 until fold.testFold._2) {
        val sys = classifier.classOf(dataset.mkDatum(i))
        val gold = dataset.labels(i)
        output += Tuple2(dataset.labelLexicon.get(gold), sys)
      }
    }

    output.toList
  }

  def sortFeaturesByFrequency[L, F](dataset:Dataset[L, F]):Counter[Int] = {
    val featCounts = new Counter[Int]()
    for(row <- 0 until dataset.size) {
      val fs = dataset.featuresCounter(row)
      for(f <- fs.keySet) {
        featCounts.incrementCount(f)
      }
    }
    featCounts
  }

  def sortFeaturesByInformativeness[L, F](dataset:Dataset[L, F], minFreq:Int):Counter[Int] = {
    // keep only features with count > minFreq
    val featCounts = new Counter[Int]()
    for(row <- 0 until dataset.size) {
      val fs = dataset.featuresCounter(row)
      for(f <- fs.keySet) {
        featCounts.incrementCount(f)
      }
    }
    val frequentFeatures = new mutable.HashSet[Int]()
    for(f <- featCounts.keySet) {
      if(featCounts.getCount(f) > minFreq)
        frequentFeatures += f
    }
    logger.info(s"Using ${frequentFeatures.size} out of ${dataset.featureLexicon.size} features with count > $minFreq.")

    val rowsWithTerm = new Counter[Int]()
    val rowsWithoutTerm = new Counter[Int]()
    val labelsWithTerm = new mutable.HashMap[Int, Counter[Int]]()
    val labelsWithoutTerm = new mutable.HashMap[Int, Counter[Int]]()

    for(row <- 0 until dataset.size) {
      val l = dataset.labels(row)
      val fs = dataset.featuresCounter(row)

      for(f <- fs.keySet) {
        rowsWithTerm.incrementCount(f)
        if(! labelsWithTerm.contains(f))
          labelsWithTerm.put(f, new Counter[Int]())
        labelsWithTerm(f).incrementCount(l)
      }

      for(nf <- frequentFeatures) {
        if(! fs.keySet.contains(nf)) {
          rowsWithoutTerm.incrementCount(nf)
          if(! labelsWithoutTerm.contains(nf))
            labelsWithoutTerm.put(nf, new Counter[Int]())
          labelsWithoutTerm(nf).incrementCount(l)
        }
      }

      if(row % 100 == 0)
        logger.debug(s"Processed $row datums out of ${dataset.size}.")
    }


    val c = new Counter[Int]
    var fc = 0
    for(fi <- frequentFeatures) {
      c.setCount(fi, informationGain(
        rowsWithTerm.getCount(fi),
        rowsWithoutTerm.getCount(fi),
        labelsWithTerm.getOrElse(fi, new Counter[Int]),
        labelsWithoutTerm.getOrElse(fi, new Counter[Int]),
        dataset.size,
        dataset.labelLexicon.size))
      fc += 1
      if(fc % 100 == 0)
        logger.debug(s"Processed $fc out of ${frequentFeatures.size} features.")
    }
    c
  }

  /**
   * This computes the IG formula from (Yang and Pedersen, 1997)
   * However, this skips the first of the formula, which independent of the term t
   */
  def informationGain[L, F](
    rowsWithTerm:Double,
    rowsWithoutTerm:Double,
    labelsWithTerm:Counter[Int],
    labelsWithoutTerm:Counter[Int],
    ND:Int,
    NL:Int):Double = {

    var probWithTerm = 0.0
    var probWithoutTerm = 0.0
    for(l <- 0 until NL) {
      val probWith = labelsWithTerm.getCount(l) / rowsWithTerm
      probWithTerm += probWith * math.log(probWith)
      val probWithout = labelsWithoutTerm.getCount(l) / rowsWithoutTerm
      probWithoutTerm += probWithout * math.log(probWithout)
    }

    val ig =
      probWithTerm * rowsWithTerm.toDouble / ND.toDouble +
      probWithoutTerm * rowsWithoutTerm.toDouble / ND.toDouble

    ig
  }

}

class ScaleRange[F] extends Serializable {
  var mins = new Counter[F]()
  var maxs = new Counter[F]()

  def update(key:F, v:Double): Unit = {
    if(! mins.contains(key) || v < mins.getCount(key))
      mins.setCount(key, v)
    if(! maxs.contains(key) || v > maxs.getCount(key))
      maxs.setCount(key, v)
  }

  def contains(key:F): Boolean = mins.contains(key)
  def min(key:F): Double = mins.getCount(key)
  def max(key:F): Double = maxs.getCount(key)

  def saveTo(w:Writer): Unit = {
    mins.saveTo(w)
    maxs.saveTo(w)
  }
}

object ScaleRange {
  def loadFrom[F](r:Reader):ScaleRange[F] = {
    val sc = new ScaleRange[F]
    sc.mins = Counter.loadFrom[F](r)
    sc.maxs = Counter.loadFrom[F](r)
    sc
  }
}


