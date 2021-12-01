package org.clulab.learning

import org.clulab.utils.Files
import org.slf4j.LoggerFactory
import de.bwaldvogel.liblinear._
import org.clulab.struct.Counter
import org.clulab.struct.Lexicon
import scala.collection.mutable.ArrayBuffer
import LiblinearRegression.logger
import java.io._

/**
  * Wrapper for liblinear regression, including LR and linear SVM
  * User: mihais, danebell
  * Date: 11/15/17
  */
class LiblinearRegression[F](
                                 val solverType: SolverType = SolverType.L2R_L2LOSS_SVR,
                                 val C: Double = 1.0,
                                 val eps: Double = 0.01,
                                 val p: Double = 0.1,
                                 val bias: Boolean = false) extends Regression[F] with Serializable {

  /** Model learned during training */
  private var model:Model = null

  /**
    * Index of the bias feature
    * If used, this is always the last feature (i.e., largest index)
    **/
  private var biasFeatureIndex:Int = -1

  /** Feature lexicon */
  private var featureLexicon:Option[Lexicon[F]] = None

  override def scoreOf(d:Datum[Double, F]): Double = {
    val features = datumToFeatures(d)
    val predictedValue = Linear.predict(model, features)
    predictedValue
  }

  override def train(dataset: RegDataset[F], indices:Array[Int]): Unit = {
    val problem = new Problem()
    problem.l = indices.length
    logger.debug(s"Using ${problem.l} datums.")
    problem.n = bias match {
      case true => dataset.numFeatures + 1
      case false => dataset.numFeatures
    }
    logger.debug(s"Using ${problem.n} features.")
    problem.bias = bias match {
      case true => 1.0
      case false => -1.0
    }
    logger.debug(s"Using bias = ${problem.bias}")
    // set the labels
    problem.y = new Array[Double](problem.l)
    for(i <- 0 until problem.l)
      problem.y(i) = dataset.labels(indices(i))

    // set the datums
    problem.x = new Array[Array[Feature]](problem.l)
    featureLexicon = Some(Lexicon(dataset.featureLexicon))
    assert(problem.l == indices.length)
    if(bias) {
      biasFeatureIndex = convertToLiblinearFeaturesIndices(featureLexicon.get.size)
      logger.debug("Bias feature index: " + biasFeatureIndex)
    }
    dataset match {
      case rvfDataset:RVFRegDataset[F] =>
        for(i <- indices.indices) {
          problem.x(i) = rvfDataToFeatures(rvfDataset.features(indices(i)), rvfDataset.values(indices(i)), sorted = true)
        }

      case bvfDataset:BVFRegDataset[F] =>
        for(i <- indices.indices) {
          problem.x(i) = bvfDataToFeatures(bvfDataset.features(indices(i)))
        }
    }
    /*
    for(i <- 0 until problem.x.length) {
      logger.debug(s"Datum #$i: " + datumToString(problem.y(i), problem.x(i)))
    }
    */

    // ... and train
    val parameter = new Parameter(solverType, C, eps, p)
    model = Linear.train(problem, parameter)

    logger.debug(s"Model contains ${model.getNrFeature} features.")
  }

  def getWeights(verbose:Boolean = false): Counter[F] = {
    val nrF = model.getNrFeature
    val ws = model.getFeatureWeights
    if(verbose) for(i <- 0 until ws.length) logger.debug(s"Weight #$i = ${ws(i)}")

    val weights = new Counter[F]()

    for(fi <- 0 until nrF) {
      val f = featureLexicon.get.get(fi)
      val w = ws(fi)
      weights.setCount(f, w)
    }

    weights
  }

  /** Add 1: our feature indices start at 0, but liblinear's start at 1! */
  private def convertToLiblinearFeaturesIndices(i:Int): Int = i + 1
  // private def convertToOutFeatureIndices(i:Int): Int = i - 1

  /*
  private def datumToString(y:Double, x:Array[Feature]): String = {
    val os = new StringBuilder
    os.append(y)
    for(f <- x) {
      os.append(" ")
      os.append(f.getIndex + ":" + f.getValue)
    }
    os.toString()
  }
  */

  private def bvfDataToFeatures(feats:Array[Int]): Array[Feature] = {
    // some of these discrete features may repeat to indicate values larger than 1; count each feature
    // we take advantage of the fact that features MUST be sorted in the dataset here
    var size = 0
    var prev = -1
    var i = 0
    while(i < feats.length) {
      if(feats(i) != prev) size += 1
      prev = feats(i)
      i += 1
    }
    if(bias) size += 1
    i = 0
    prev = -1
    var j = 0
    val features = new Array[Feature](size)
    while(i < feats.length) {
      if(feats(i) != prev) {
        features(j) = new FeatureNode(convertToLiblinearFeaturesIndices(feats(i)), 1.0)
        j += 1
      } else {
        // we've seen the same feature again; increment its value
        features(j - 1).setValue(features(j - 1).getValue + 1.0)
      }
      prev = feats(i)
      i += 1
    }
    // add the bias feature if necessary
    if(bias) {
      features(j) = new FeatureNode(biasFeatureIndex, 1.0)
    }
    /*
    // sanity check
    for(i <- 0 until features.size - 1) {
      if(features(i).getIndex > features(i + 1).getIndex) {
        throw new RuntimeException("ERROR: features not sorted " + features(i).getIndex + " vs. " + features(i + 1).getIndex)
      }
    }
    */
    // features are already sorted in the dataset; no need to sort here
    features
  }

  private def rvfDataToFeatures(
                                 feats:Array[Int],
                                 vals:Array[Double],
                                 sorted:Boolean): Array[Feature] = {
    // Unlike BVF features, RVF features are not supposed to repeat, because values are stored separately!
    var size = feats.length
    if(bias) size += 1
    val features = new Array[Feature](size)
    var i = 0
    while(i < feats.length) {
      features(i) = new FeatureNode(convertToLiblinearFeaturesIndices(feats(i)), vals(i))
      i += 1
    }
    // add the bias feature if necessary
    if(bias) {
      features(i) = new FeatureNode(biasFeatureIndex, 1.0)
    }
    // features are already sorted in the dataset but may not be sorted in a datum; sort if necessary
    if(! sorted) features.sortBy(_.getIndex)
    else features
  }

  private def datumToFeatures(d:Datum[Double, F]): Array[Feature] = {
    d match {
      case rvf:RVFDatum[Double, F] =>
        val fs = new ArrayBuffer[Int]()
        val vs = new ArrayBuffer[Double]()
        for(f <- rvf.featuresCounter.keySet) {
          val of = featureLexicon.get.get(f)
          if(of.isDefined) {
            fs += of.get
            vs += rvf.featuresCounter.getCount(f)
          }
        }
        rvfDataToFeatures(fs.toArray, vs.toArray, sorted = false)

      case bvf:BVFDatum[Double, F] =>
        val fs = new ArrayBuffer[Int]
        for(f <- bvf.features){
          val of = featureLexicon.get.get(f)
          if(of.isDefined) fs += of.get
        }
        bvfDataToFeatures(fs.sorted.toArray)

      case _ =>
        throw new RuntimeException("ERROR: do not know how to process this datum type!")

    }
  }

  /** Saves the current model to a file */
  override def saveTo(w:Writer): Unit = {
    val writer = Files.toPrintWriter(w)
    featureLexicon.get.saveTo(writer)
    writer.append(s"$bias $biasFeatureIndex\n")
    Linear.saveModel(writer, model)
  }
}

/**
  * L2-regularized L2-loss support vector regression (primal)
  */
class LinearSVMRegression[F] (
                                  C: Double = 1.0,
                                  eps: Double = 0.01,
                                  p: Double = 0.1,
                                  bias: Boolean = false)
  extends LiblinearRegression[F](SolverType.L2R_L2LOSS_SVR, C, p, eps, bias)

/**
  * L2-regularized L2-loss support vector regression (dual)
  */
class LinearSVMRegressionDual[F] (
                                  C: Double = 1.0,
                                  eps: Double = 0.01,
                                  p: Double = 0.1, //not used
                                  bias: Boolean = false)
  extends LiblinearRegression[F](SolverType.L2R_L2LOSS_SVR_DUAL, C, p, eps, bias)

/**
  * L2-regularized L1-loss support vector regression (dual)
  */
class L1LinearSVMRegression[F] (
                                  C: Double = 1.0,
                                  eps: Double = 0.01,
                                  p: Double = 0.1, // not used
                                  bias: Boolean = false)
  extends LiblinearRegression[F](SolverType.L2R_L1LOSS_SVR_DUAL, C, p, eps, bias)


object LiblinearRegression {
  val logger = LoggerFactory.getLogger(this.getClass)

  def loadFrom[F](fileName:String):LiblinearRegression[F] = {
    val r = new BufferedReader(new FileReader(fileName))
    val c = loadFrom[F](r)
    r.close()
    c
  }

  def loadFrom[F](r:Reader): LiblinearRegression[F] = {
    val reader = Files.toBufferedReader(r)
    val fl = Lexicon.loadFrom[F](reader)
    val bits = reader.readLine().split("\\s+")
    val bias = bits(0).toBoolean
    val biasFeatureIndex = bits(1).toInt
    val c = new LiblinearRegression[F](SolverType.L2R_LR, 1.0, 0.01, 0.1, bias) // only bias matters at prediction time
    c.biasFeatureIndex = biasFeatureIndex
    c.featureLexicon = Some(fl)
    c.model = Linear.loadModel(reader)
    c
  }
}
