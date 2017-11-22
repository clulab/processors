package org.clulab.learning

import libsvm._
import scala.collection.mutable.ArrayBuffer
import org.clulab.struct.Lexicon
import org.slf4j.LoggerFactory
import LibSvmRegression.logger
import java.io._

/**
  * Wrapper for libsvm regression
  * User: mihais, dfried, danebell
  * Date: 11/20/2017
  */
class LibSvmRegression[F](val parameters: svm_parameter) extends Regression[F] with Serializable {
  def this(svmType: SvmType = EpsilonSVR,
           kernelType: KernelType,
           degree: Int = 3, // for poly
           gamma: Double = 0, // for poly/rbf/sigmoid. If 0, sets to 1 / num feats
           coef0: Double = 0, // for poly/sigmoid
           C: Double = 1,
           nu: Double = 0.5,
           p: Double = 0.1,
           eps: Double = 1e-3,
           shrinking: Boolean = true,
           probability: Boolean = true,
           cacheSize: Int = 100) = {
    this(
      LibSvmRegression
        .makeParameters(
          svmType, kernelType, degree,
          gamma, coef0, C, nu, p, eps,
          shrinking, probability,
          cacheSize
        )
    )
  }

  private var problem: svm_problem = null
  private var model: svm_model = null

  /** Feature lexicon */
  private var featureLexicon:Option[Lexicon[F]] = None

  /**
    * Trains a classifier, using only the datums specified in indices
    * indices is useful for bagging
    */
  def train(dataset: RegDataset[F], indices: Array[Int]): Unit = {
    problem = new svm_problem
    problem.l = indices.length
    logger.debug(s"Using ${problem.l} datums.")
    /*
    problem.n = bias match {
      case true => dataset.numFeatures + 1
      case false => dataset.numFeatures
    }
    logger.debug(s"Using ${problem.n} features.")
    */
    /*
    problem.bias = bias match {
      case true => 1.0
      case false => -1.0
    }
    logger.debug(s"Using bias = ${problem.bias}")
    */
    // set the labels
    problem.y = new Array[Double](problem.l)
    for(i <- 0 until problem.l)
      problem.y(i) = dataset.labels(indices(i))

    // set the datums
    problem.x = new Array[Array[svm_node]](problem.l)
    featureLexicon = Some(Lexicon(dataset.featureLexicon))
    assert(problem.l == indices.length)
    /*
    if(bias) {
      biasFeatureIndex = convertToLiblinearFeaturesIndices(featureLexicon.get.size)
      logger.debug("Bias feature index: " + biasFeatureIndex)
    }
    */
    dataset match {
      case rvfDataset:RVFRegDataset[F] =>
        for(i <- indices.indices) {
          problem.x(i) = rvfDataToNodes(rvfDataset.features(indices(i)), rvfDataset.values(indices(i)), sorted = true)
        }

      case bvfDataset:BVFRegDataset[F] =>
        for(i <- indices.indices) {
          problem.x(i) = bvfDataToNodes(bvfDataset.features(indices(i)))
        }

    }
    /*
    for(i <- 0 until problem.x.length) {
      logger.debug(s"Datum #$i: " + datumToString(problem.y(i), problem.x(i)))
    }
    */

    // possibly set gamma based on # features
    if (parameters.gamma == 0 && featureLexicon.get.size > 0) {
      parameters.gamma = 1.0 / featureLexicon.get.size
    }
    // check parameters
    val error_msg = svm.svm_check_parameter(problem, parameters)
    if (error_msg != null) {
      throw new Exception(error_msg)
    }
    // ... and train
    model = svm.svm_train(problem, parameters)
  }

  /**
    * Returns the scores of all possible labels for this datum
    * Convention: if the classifier can return probabilities, these must be probabilities
    **/
  override def scoreOf(d:Datum[Double, F]): Double = {
    val nodes = datumToNodes(d)
    val predictedValue = svm.svm_predict(model, nodes)
    predictedValue
  }

  /** Saves the current model to a file */
  override def saveTo(writer:Writer) { throw new RuntimeException("ERROR: saving to Writer not supported yet!") }

  override def saveTo(fn:String) {
    val os = new ObjectOutputStream(new FileOutputStream(fn))
    os.writeObject(this)
    os.close()
  }

  private def convertToLibsvmFeaturesIndices(i: Int) = i + 1
  // private def convertToOutputFeaturesIndices(i: Int) = i - 1

  private def bvfDataToNodes(feats:Array[Int]): Array[svm_node] = {
    // modified from LibLinearRegression code
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
    // if(bias) size += 1
    i = 0
    prev = -1
    var j = 0
    val nodes = new Array[svm_node](size)
    while(i < feats.length) {
      if(feats(i) != prev) {
        nodes(j) = new svm_node { index = convertToLibsvmFeaturesIndices(feats(i)); value =  1.0 }
        j += 1
      } else {
        // we've seen the same feature again; increment its value
        nodes(j - 1).value += 1.0
      }
      prev = feats(i)
      i += 1
    }
    /*
    // add the bias feature if necessary
    if(bias) {
      features(j) = new FeatureNode(biasFeatureIndex, 1.0)
    }
    */
    nodes
  }

  private def rvfDataToNodes(feats:Array[Int],
                             vals:Array[Double],
                             sorted:Boolean): Array[svm_node] = {
    // Unlike BVF features, RVF features are not supposed to repeat, because values are stored separately!
    val size = feats.length
    // if(bias) size += 1
    val features = new Array[svm_node](size)
    var i = 0
    while(i < feats.length) {
      features(i) = new svm_node { index = convertToLibsvmFeaturesIndices(feats(i)); value = vals(i) }
      i += 1
    }
    // add the bias feature if necessary
    /*
    if(bias) {
      features(i) = new svm_node { index = biasFeatureIndex; value = 1.0 }
    }
    */
    // features are already sorted in the dataset but may not be sorted in a datum; sort if necessary
    if(! sorted) features.sortBy(_.index)
    else features
  }

  private def datumToNodes(d:Datum[Double, F]): Array[svm_node] = {
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
        rvfDataToNodes(fs.toArray, vs.toArray, sorted = false)

      case bvf:BVFDatum[Double, F] =>
        val fs = new ArrayBuffer[Int]
        for(f <- bvf.features){
          val of = featureLexicon.get.get(f)
          if(of.isDefined) fs += of.get
        }
        bvfDataToNodes(fs.sorted.toArray)

      case _ =>
        throw new RuntimeException("ERROR: do not know how to process this datum type!")

    }
  }
}

object LibSvmRegression {
  val logger = LoggerFactory.getLogger(this.getClass)

  def loadFrom[F](fileName:String):LibSvmRegression[F] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[LibSvmRegression[F]]
    is.close()
    c
  }

  def makeParameters(
                      svmType: SvmType,
                      kernelType: KernelType,
                      degree: Int, // for poly
                      gamma: Double, // for poly/rbf/sigmoid
                      coef0: Double, // for poly/sigmoid
                      C: Double, // cost
                      nu: Double, // for NU_SVR
                      p: Double, // for EPSILON_SVR
                      eps: Double, // "tolerance of termination criterion"
                      shrinking: Boolean,
                      probability: Boolean,
                      cache_size : Int) = {
    val params = new svm_parameter

    params.svm_type = svmType match {
      case EpsilonSVR => svm_parameter.EPSILON_SVR
      case NuSVR => svm_parameter.NU_SVR
    }

    params.kernel_type = kernelType match {
      case LinearKernel => svm_parameter.LINEAR
      case PolynomialKernel => svm_parameter.POLY
      case RBFKernel => svm_parameter.RBF
      case SigmoidKernel => svm_parameter.SIGMOID
    }

    params.degree = degree
    params.gamma = gamma
    params.coef0 = coef0
    params.C = C
    params.nu = nu
    params.p = p
    params.eps = eps
    params.shrinking = if (shrinking) 1 else 0
    params.probability = if (probability) 1 else 0
    params.cache_size = cache_size
    params
  }
}

class LibSvmEpsilonRegression[F](val p: svm_parameter) extends LibSvmRegression[F](p) {
  def this(kernelType: KernelType,
           degree: Int = 3, // for poly
           gamma: Double = 0, // for poly/rbf/sigmoid. If 0, sets to 1 / num feats
           coef0: Double = 0, // for poly/sigmoid
           C: Double = 1,
           nu: Double = 0.5, // not used
           p: Double = 0.1,
           eps: Double = 1e-3,
           shrinking: Boolean = true,
           probability: Boolean = true,
           cacheSize: Int = 100) = {
    this(
      LibSvmRegression
        .makeParameters(
          EpsilonSVR, kernelType, degree,
          gamma, coef0, C, nu, p, eps,
          shrinking, probability,
          cacheSize
        )
    )
  }
}

class LibSvmNuRegression[F](val p: svm_parameter) extends LibSvmRegression[F](p) {
  def this(kernelType: KernelType,
           degree: Int = 3, // for poly
           gamma: Double = 0, // for poly/rbf/sigmoid. If 0, sets to 1 / num feats
           coef0: Double = 0, // for poly/sigmoid
           C: Double = 1,
           nu: Double = 0.5,
           p: Double = 0.1, // not used
           eps: Double = 1e-3,
           shrinking: Boolean = true,
           probability: Boolean = true,
           cacheSize: Int = 100) = {
    this(
      LibSvmRegression
        .makeParameters(
          NuSVR, kernelType, degree,
          gamma, coef0, C, nu, p, eps,
          shrinking, probability,
          cacheSize
        )
    )
  }
}
