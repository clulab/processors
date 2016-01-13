package edu.arizona.sista.learning

import java.io._
import scala.collection.mutable.ArrayBuffer
import edu.uci.jforestsx.applications.RankingApp
import edu.uci.jforestsx.learning.trees.Ensemble
import java.util.Properties
import edu.uci.jforestsx.config.TrainingConfig
import edu.arizona.sista.utils.{StringUtils, MathUtils}
import org.slf4j.LoggerFactory
import JForestsRankingClassifier.logger
import edu.arizona.sista.struct.Lexicon
import edu.uci.jforestsx.learning.LearningUtils
import edu.uci.jforestsx.input._
import scala.Serializable
import edu.uci.jforestsx.dataset.Feature
import scala.collection.mutable
import scala.util.Random


/**
 * Wrapper for jforests (the boosted decision trees software) in ranking mode
 * User: mihais
 * Date: 11/26/13
 */
class JForestsRankingClassifier[F] (
                                     val workingDir:String = ".",
                                     val trainFilePrefix:String = "train",
                                     val treesNumLeaves:Int = 7,
                                     val treesMinInstancePercentagePerLeaf:Double = 0.25,
                                     val treesFeatureSampling:Double = 0.3,
                                     val boostingLearningRate:Double = 0.05,
                                     val boostingSubSampling:Double = 0.3,
                                     val boostingNumTrees:Int = 2000,
                                     val trainFraction:Double = 0.80,
                                     val keepIntermediateFiles:Boolean = false)
  extends RankingClassifier[F] with Serializable {

  def this(props:Properties) =
    this(
      props.getProperty("workingDir", "."),
      props.getProperty("trainFilePrefix", "train"),
      StringUtils.getInt(props, "treesNumLeaves", 7),
      StringUtils.getDouble(props, "treesMinInstancePercentagePerLeaf", 0.25),
      StringUtils.getDouble(props, "treesFeatureSampling", 0.3),
      StringUtils.getDouble(props, "boostingLearningRate", 0.05),
      StringUtils.getDouble(props, "boostingSubSampling", 0.3),
      StringUtils.getInt(props, "boostingNumTrees", 2000),
      StringUtils.getDouble(props, "trainFraction", 0.90),
      StringUtils.getBool(props, "keepIntermediateFiles", false))

  var ensemble:Option[Ensemble] = None
  var featureLexicon:Option[Lexicon[F]] = None
  var featureAnalyzer:Option[FeatureAnalyzer] = None

  def train(dataset:RankingDataset[F], spans:Option[Iterable[(Int, Int)]] = None) {

    //
    // cleanup working dir
    //
    rmTempFiles()

    //
    // generate training and validation datasets
    //
    val datumIndices = extractIndices(spans.getOrElse(mkFullFold(dataset.size)))
    val randomizedIndices = MathUtils.randomize(datumIndices, new Random(0))
    // the first trainFraction datums go to the training fold; the rest to the validationFold
    assert(trainFraction > 0 && trainFraction < 1)
    val endTrain = (trainFraction * randomizedIndices.length.toDouble).toInt
    val trainIndices = sub(randomizedIndices, 0, endTrain)
    val validationIndices = sub(randomizedIndices, endTrain, randomizedIndices.length)
    logger.info(s"Training with ${trainIndices.length} datums, and validating with ${validationIndices.length} datums.")
    logger.debug("Training indices: " + trainIndices.toList)
    logger.debug("Validation indices: " + validationIndices.toList)

    // creates train file in svm_rank format
    val txtTrain = workingDir + File.separator + trainFilePrefix + ".train.txt"
    mkTrainFile(txtTrain, dataset, trainIndices)
    val txtValid = workingDir + File.separator + trainFilePrefix + ".valid.txt"
    mkTrainFile(txtValid, dataset, validationIndices)

    // converts svm_rank format to the jforests internal format
    val trainFiles = new ArrayBuffer[String]()
    trainFiles += trainFilePrefix + ".train.txt"
    trainFiles += trainFilePrefix + ".valid.txt"
    convertToBinForRanking(trainFiles.toArray, workingDir)

    //
    // prepare configuration for training
    //
    val trainProperties = new Properties()
    trainProperties.setProperty(TrainingConfig.TRAIN_FILENAME,
      workingDir + File.separator + trainFilePrefix + ".train.bin")
    trainProperties.setProperty(TrainingConfig.VALID_FILENAME,
      workingDir + File.separator + trainFilePrefix + ".valid.bin")
    // from TreesConfig
    trainProperties.setProperty("trees.num-leaves", treesNumLeaves.toString)
    trainProperties.setProperty("trees.min-instance-percentage-per-leaf", treesMinInstancePercentagePerLeaf.toString)
    trainProperties.setProperty("trees.feature-sampling", treesFeatureSampling.toString)
    // from GradientBoostingConfig
    trainProperties.setProperty("boosting.learning-rate", boostingLearningRate.toString)
    trainProperties.setProperty("boosting.sub-sampling", boostingSubSampling.toString)
    trainProperties.setProperty("boosting.num-trees", boostingNumTrees.toString)
    // from TrainingConfig
    trainProperties.setProperty("learning.algorithm", "LambdaMART-RegressionTree")
    trainProperties.setProperty("learning.evaluation-metric", "NDCG")
    trainProperties.setProperty("params.print-intermediate-valid-measurements", "true")

    //
    // train the classifier
    //
    val app = new RankingApp
    ensemble = Some(app.run(trainProperties))
    featureLexicon = Some(Lexicon(dataset.featureLexicon))
    featureAnalyzer = Some(new FeatureAnalyzer)
    featureAnalyzer.get.loadFeaturesFromFile(workingDir + File.separator + "jforests-feature-stats.txt")


    // cleanup
    if(! keepIntermediateFiles) rmTempFiles()
  }

  private def rmTempFiles() {
    new File(workingDir + File.separator + "jforests-feature-stats.txt").delete()
    new File(workingDir + File.separator + "jforests-discrete-" + trainFilePrefix + ".train.txt").delete()
    new File(workingDir + File.separator + "jforests-discrete-" + trainFilePrefix + ".valid.txt").delete()
    new File(workingDir + File.separator + trainFilePrefix + ".train.bin").delete()
    new File(workingDir + File.separator + trainFilePrefix + ".train.txt").delete()
    new File(workingDir + File.separator + trainFilePrefix + ".valid.bin").delete()
    new File(workingDir + File.separator + trainFilePrefix + ".valid.txt").delete()
  }

  private def mkTrainFile(fn:String, d:RankingDataset[F], indices:Array[Int]):Int = {
    val pw = new PrintWriter(new FileWriter(fn))

    val labelValues = new mutable.HashSet[Int]()
    for(i <- indices) {
      val queryLabels = d.labels(i)
      for(j <- 0 until d.querySize(i)) {
        val l = queryLabels(j)
        labelValues += l
      }
    }
    val sortedLabels = labelValues.toList.sorted
    val labelMap = new mutable.HashMap[Int, Int]()
    if(sortedLabels.size <= 5) {
      for(i <- 0 until sortedLabels.length)
        labelMap += sortedLabels(i) -> i
    } else {
      // TODO
      throw new RuntimeException("TODO: support more than 5 labels by binning")
    }
    assert(labelMap.size > 0)

    var n = 0
    for(i <- indices) {
      val qid = i + 1
      val queryLabels = d.labels(i)

      for(j <- 0 until d.querySize(i)) {
        val l = queryLabels(j)
        val sl = labelMap.get(l).get
        val fs = d.featuresCounter(i, j)
        val fids = fs.keySet.toList.sorted

        pw.print(sl + " qid:" + qid)
        // our feature ids start at 0, but svm_rank requires features to start at 1
        fids.foreach(fid => pw.print(" " + (fid + 1) + ":" + fs.getCount(fid)))
        pw.println()
      }
      n += 1
    }
    pw.close()
    n
  }

  def scoresOf(queryDatums:Iterable[Datum[Int, F]]):Iterable[Double] = {
    val scores = new ArrayBuffer[Double]()

    for(datum <- queryDatums) {
      //logger.debug(s"Classifying datum ${datum}")
      val rawFeatures = toFeatureValuePairs(datum, featureLexicon.get)
      val jforestsFeatures = convertToJForestsFeatures(rawFeatures, featureAnalyzer.get)
      val score = LearningUtils.computeScore(ensemble.get, jforestsFeatures)
      //logger.debug(s"Regression score: $score")
      scores += score
    }

    scores.toArray
  }

  def saveTo(fileName:String) {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    os.writeObject(this)
    os.close()
  }

  def displayModel(pw:PrintWriter) {
    pw.println(ensemble)
  }

  def extractIndices(trainFolds:Iterable[(Int, Int)]):Array[Int] = {
    val indices = new ArrayBuffer[Int]()
    for(fold <- trainFolds) {
      for(i <- fold._1 until fold._2) {
        indices += i
      }
    }
    indices.toArray
  }

  def mkFullFold(size:Int): Iterable[(Int, Int)] = {
    val folds = new Array[(Int, Int)](1)
    folds(0) = new Tuple2(0, size)
    folds
  }

  def sub(a:Array[Int], start:Int, end:Int):Array[Int] = {
    val b = new ArrayBuffer[Int]()
    for(i <- start until end) b += a(i)
    b.toArray
  }

  def convertToBinForRanking(src:Array[String], workingDir:String) {
    val conv = new RankingRaw2BinConvertor()
    conv.convert(workingDir, src)
  }

  def convertToBinForClassification(src:Array[String], workingDir:String) {
    val conv = new Raw2BinConvertor()
    conv.convert(workingDir, src)
  }

  def convertToBinForRanking(src:String, workingDir:String) {
    val conv = new RankingRaw2BinConvertor()
    val files = new ArrayBuffer[String]()
    files += src
    conv.convert(workingDir, files.toArray)
  }

  def convertToJForestsFeatures(rawFeatures:Array[FeatureValuePair], featureAnalyzer:FeatureAnalyzer):Array[Feature] = {
    val discreteFeatures:Array[FeatureValuePair] =
      DiscreteSparseTextFileGenerator.convert(rawFeatures, featureAnalyzer)
    BinaryFileGenerator.convert(discreteFeatures, featureAnalyzer)
  }

  def toFeatureValuePairs(datum:Datum[Int, F], featureLexicon:Lexicon[F]):Array[FeatureValuePair] = {
    val c = datum.featuresCounter
    val fvs = new ArrayBuffer[FeatureValuePair]()
    for(featName <- datum.features) {
      val featIdx = featureLexicon.get(featName)
      if(featIdx.isDefined) {
        val fv = new FeatureValuePair
        // jforests use feature indices starting at 0, same as us!
        fv.featureIndex = featIdx.get
        fv.featureValue = c.getCount(featName)
        fvs += fv
      }
    }
    fvs.toArray.sortBy(_.featureIndex)
  }

}

object JForestsRankingClassifier {
  val logger = LoggerFactory.getLogger(classOf[JForestsRankingClassifier[String]])

  def loadFrom[F](fileName:String):JForestsRankingClassifier[F] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[JForestsRankingClassifier[F]]
    is.close()
    c
  }
}
