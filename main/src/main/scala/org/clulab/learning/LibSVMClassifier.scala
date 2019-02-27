package org.clulab.learning

import org.clulab.struct.Counter
import libsvm._

import scala.collection.mutable.ArrayBuffer
import org.clulab.struct.Lexicon
import org.slf4j.LoggerFactory
import LibSVMClassifier.logger
import java.io._

import org.clulab.learning._
import org.clulab.utils.Serializer



/**
  * Modified from mihais's Liblinear wrapper by dfried on 5/2/14
  * Further modified by enrique on 5/15/18
  */
class LibSVMClassifier[L, F](val parameters: svm_parameter) extends Classifier[L,F] with Serializable {
  def this(kernelType: KernelType,
           degree: Int = 3, // for poly
           gamma: Double = 0, // for poly/rbf/sigmoid. If 0, sets to 1 / num feats
           coef0: Double = 0, // for poly/sigmoid
           C: Double = 1,
           eps: Double = 1e-3,
           shrinking: Boolean = true,
           probability: Boolean = true,
           cacheSize: Int = 100) =
    this(LibSVMClassifier.makeParameters(kernelType, degree, gamma, coef0, C, eps, shrinking, probability, cacheSize))

  private var problem: svm_problem = null
  private var model: svm_model = null

  /** Feature lexicon */
  private var featureLexicon:Option[Lexicon[F]] = None

  /** Label lexicon */
  private var labelLexicon:Option[Lexicon[L]] = None

  /**
    * Trains a classifier, using only the datums specified in indices
    * indices is useful for bagging
    */
  override def train(dataset: Dataset[L, F], indices: Array[Int]): Unit = train(dataset, indices, None)

  /**
    * Trains a classifier, using only the datums specified in indices
    * indices is useful for bagging
    * Class weights allow for balancing of not evenly distributed labels by scaling the regularization parameter (C)
    */
  def train(dataset: Dataset[L, F], indices: Array[Int], classWeights:Option[Map[L, Double]]): Unit = {
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
      problem.y(i) = dataset.labels(indices(i)).toDouble

    // set the datums
    problem.x = new Array[Array[svm_node]](problem.l)
    featureLexicon = Some(Lexicon(dataset.featureLexicon))
    labelLexicon = Some(Lexicon(dataset.labelLexicon))
    assert(problem.l == indices.length)
    /*
    if(bias) {
      biasFeatureIndex = convertToLiblinearFeaturesIndices(featureLexicon.get.size)
      logger.debug("Bias feature index: " + biasFeatureIndex)
    }
    */
    dataset match {
      case rvfDataset:RVFDataset[L, F] => {
        for(i <- 0 until indices.length) {
          problem.x(i) = rvfDataToNodes(rvfDataset.features(indices(i)), rvfDataset.values(indices(i)), sorted = true)
        }
      }
      case bvfDataset:BVFDataset[L, F] => {
        for(i <- 0 until indices.length) {
          problem.x(i) = bvfDataToNodes(bvfDataset.features(indices(i)))
        }
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

    // Add class weights, if provided
    classWeights match {
      case Some(weights) => {
        // Class weights will be marshaled to libsvm's parameter data structure
        val nr_weights = weights.size
        val (weight_labels, weight_values) = weights.map{
          case (label, value) =>  (dataset.labelLexicon.get(label).get, value)
        }.unzip

        parameters.nr_weight = nr_weights
        parameters.weight_label = weight_labels.toArray
        parameters.weight = weight_values.toArray
      }
      case None => Unit
    }

    // ... and train
    model = svm.svm_train(problem, parameters)

    logger.debug(s"Model contains ${model.nr_class} classes.")
    // logger.debug(s"Model contains ${model.getNrFeature} features.")
  }

  /** Returns the argmax for this datum */
  override def classOf(d:Datum[L, F]): L = {
    val nodes = datumToNodes(d)
    val li = svm.svm_predict(model, nodes)
    labelLexicon.get.get(li.toInt)
  }

  /**
    * Returns the scores of all possible labels for this datum
    * Convention: if the classifier can return probabilities, these must be probabilities
    **/
  override def scoresOf(d:Datum[L, F]): Counter[L] = {
    val nodes = datumToNodes(d)
    val probs = new Array[Double](model.nr_class)
    svm.svm_predict_probability(model, nodes, probs)
    val probabilities = new Counter[L]
    for(i <- 0 until model.nr_class) {
      probabilities.setCount(labelLexicon.get.get(model.label(i)), probs(i))
    }
    probabilities
  }

  /** Saves the current model to a file */
  override def saveTo(writer:Writer) { throw new RuntimeException("ERROR: saving to Writer not supported yet!") }

  override def saveTo(fn:String) {
    Serializer.save(this, fn)
  }

  private def convertToLibsvmFeaturesIndices(i: Int) = i + 1
  // private def convertToOutputFeaturesIndices(i: Int) = i - 1

  private def bvfDataToNodes(feats:Array[Int]): Array[svm_node] = {
    // modified from LibLinearClassifier code
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

  private def datumToNodes(d:Datum[L, F]): Array[svm_node] = {
    d match {
      case rvf:RVFDatum[L, F] => {
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
      }
      case bvf:BVFDatum[L, F] => {
        val fs = new ArrayBuffer[Int]
        for(f <- bvf.features){
          val of = featureLexicon.get.get(f)
          if(of.isDefined) fs += of.get
        }
        bvfDataToNodes(fs.sorted.toArray)
      }
      case _ => {
        throw new RuntimeException("ERROR: do not know how to process this datum type!")
      }
    }
  }
}

object LibSVMClassifier {
  val logger = LoggerFactory.getLogger(classOf[LibSVMClassifier[String, String]])

  def loadFrom[L, F](fileName:String):LibSVMClassifier[L, F] = {
    Serializer.load(fileName)
  }

  def makeParameters(kernelType: KernelType,
                     degree: Int, // for poly
                     gamma: Double, // for poly/rbf/sigmoid
                     coef0: Double, // for poly/sigmoid
                     C: Double,
                     eps: Double,
                     shrinking: Boolean,
                     probability: Boolean,
                     cache_size : Int) = {
    val params = new svm_parameter
    params.svm_type = svm_parameter.C_SVC

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
    params.eps = eps
    params.shrinking = if (shrinking) 1 else 0
    params.probability = if (probability) 1 else 0
    params.cache_size = cache_size
    params
  }
}
