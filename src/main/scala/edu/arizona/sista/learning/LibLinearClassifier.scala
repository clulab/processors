package edu.arizona.sista.learning

import edu.arizona.sista.utils.{Files,MathUtils}
import org.slf4j.LoggerFactory
import de.bwaldvogel.liblinear._
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.struct.Lexicon
import scala.collection.mutable.ArrayBuffer
import LiblinearClassifier.logger
import scala.collection.mutable
import java.io._

/**
 * Wrapper for liblinear classifiers, which includes LR and linear SVM
 * Note: this only supports classification; it does not support regression by design
 * User: mihais
 * Date: 11/16/13
 */
class LiblinearClassifier[L, F](
  val solverType:SolverType = SolverType.L2R_LR,
  val C:Double = 1.0,
  val eps:Double = 0.01,
  val bias:Boolean = false) extends Classifier[L, F] with Serializable {

  /** Model learned during training */
  private var model:Model = null

  /**
   * Index of the bias feature
   * If used, this is always the last feature (i.e., largest index)
   **/
  private var biasFeatureIndex:Int = -1

  /** Feature lexicon */
  private var featureLexicon:Option[Lexicon[F]] = None

  /** Label lexicon */
  private var labelLexicon:Option[Lexicon[L]] = None

  override def classOf(d:Datum[L, F]): L = {
    val features = datumToFeatures(d)
    val li = Linear.predict(model, features)
    labelLexicon.get.get(li.toInt)
  }

  override def scoresOf(d:Datum[L, F]): Counter[L] = {
    val features = datumToFeatures(d)
    val probs = new Array[Double](model.getNrClass)
    val normedProbs: Array[Double] = if(model.isProbabilityModel) {
      Linear.predictProbability(model, features, probs)
      probs
    } else {
      Linear.predictValues(model, features, probs)
      MathUtils.softmax(probs).toArray
    }
    val probabilities = new Counter[L]
    for(i <- 0 until model.getNrClass) {
      probabilities.setCount(labelLexicon.get.get(model.getLabels()(i)), normedProbs(i))
    }
    probabilities
  }

  override def train(dataset:Dataset[L, F], indices:Array[Int]) {
    val problem = new Problem()
    problem.l = indices.length
    logger.debug(s"Using ${problem.l} datums.")
    var labelHist = new Counter[L]
    for(l <- dataset.labels)
      labelHist.incrementCount(dataset.labelLexicon.get(l))
    logger.debug(s"Label distribution: ${labelHist.toShortString}")
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
      problem.y(i) = dataset.labels(indices(i)).toDouble

    // set the datums
    problem.x = new Array[Array[Feature]](problem.l)
    featureLexicon = Some(Lexicon(dataset.featureLexicon))
    labelLexicon = Some(Lexicon(dataset.labelLexicon))
    assert(problem.l == indices.length)
    if(bias) {
      biasFeatureIndex = convertToLiblinearFeaturesIndices(featureLexicon.get.size)
      logger.debug("Bias feature index: " + biasFeatureIndex)
    }
    dataset match {
      case rvfDataset:RVFDataset[L, F] => {
        for(i <- 0 until indices.length) {
          problem.x(i) = rvfDataToFeatures(rvfDataset.features(indices(i)), rvfDataset.values(indices(i)), sorted = true)
        }
      }
      case bvfDataset:BVFDataset[L, F] => {
        for(i <- 0 until indices.length) {
          problem.x(i) = bvfDataToFeatures(bvfDataset.features(indices(i)))
        }
      }
    }
    /*
    for(i <- 0 until problem.x.length) {
      logger.debug(s"Datum #$i: " + datumToString(problem.y(i), problem.x(i)))
    }
    */

    // ... and train
    val parameter = new Parameter(solverType, C, eps)
    model = Linear.train(problem, parameter)

    logger.debug(s"Model contains ${model.getNrClass} classes.")
    logger.debug(s"Model contains ${model.getNrFeature} features.")
  }

  def getWeights(verbose:Boolean = false): Map[L, Counter[F]] = {
    val nrC = model.getNrClass
    val nrF = model.getNrFeature
    val ws = model.getFeatureWeights
    if(verbose) for(i <- 0 until ws.length) logger.debug(s"Weight #$i = ${ws(i)}")

    val weights = new mutable.HashMap[L, Counter[F]]()

    if(nrC == 2) {
      // if two classes, liblinear only stores the weights for the first class
      // the others are just 0 - first weights
      for(l <- labelLexicon.get.keySet) weights.put(l, new Counter[F])
      for(fi <- 0 until nrF) {
        val label1 = labelLexicon.get.get(model.getLabels()(0))
        val label2 = labelLexicon.get.get(model.getLabels()(1))
        val f = featureLexicon.get.get(convertToOutFeatureIndices(fi + 1))
        val w = ws(fi)
        weights.get(label1).get.setCount(f, w)
        weights.get(label2).get.setCount(f, 0.0 - w)
      }
    } else {
      // here we have weights for each class
      for(fi <- 0 until nrF) {
        val offset = fi * nrC
        for(ci <- 0 until nrC) {
          val i = offset + ci
          val w = ws(i)
          val label = labelLexicon.get.get(model.getLabels()(ci))
          val f = featureLexicon.get.get(convertToOutFeatureIndices(fi + 1))
          if(! weights.contains(label)) weights.put(label, new Counter[F])
          weights.get(label).get.setCount(f, w)
        }
      }
    }

    weights.toMap
  }

  /** Add 1: our feature indices start at 0, but liblinear's start at 1! */
  private def convertToLiblinearFeaturesIndices(i:Int): Int = i + 1
  private def convertToOutFeatureIndices(i:Int): Int = i - 1

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
    while(i < feats.size) {
      if(feats(i) != prev) size += 1
      prev = feats(i)
      i += 1
    }
    if(bias) size += 1
    i = 0
    prev = -1
    var j = 0
    val features = new Array[Feature](size)
    while(i < feats.size) {
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
    var size = feats.size
    if(bias) size += 1
    val features = new Array[Feature](size)
    var i = 0
    while(i < feats.size) {
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

  private def datumToFeatures(d:Datum[L, F]): Array[Feature] = {
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
        rvfDataToFeatures(fs.toArray, vs.toArray, sorted = false)
      }
      case bvf:BVFDatum[L, F] => {
        val fs = new ArrayBuffer[Int]
        for(f <- bvf.features){
          val of = featureLexicon.get.get(f)
          if(of.isDefined) fs += of.get
        }
        bvfDataToFeatures(fs.sorted.toArray)
      }
      case _ => {
        throw new RuntimeException("ERROR: do not know how to process this datum type!")
      }
    }
  }

  /** Saves the current model to a file */
  override def saveTo(w:Writer) {
    val writer = Files.toPrintWriter(w)
    featureLexicon.get.saveTo(writer)
    labelLexicon.get.saveTo(writer)
    writer.append(s"$bias $biasFeatureIndex\n")
    Linear.saveModel(writer, model)
  }
}

/**
 * Vanilla logistic regression with L2 regularization
 */
class LogisticRegressionClassifier[L, F] (
  C:Double = 1.0,
  eps:Double = 0.01,
  bias:Boolean = false)
  extends LiblinearClassifier[L, F](SolverType.L2R_LR, C, eps, bias)

/**
 * Linear SVM with L2 regularization
 */
class LinearSVMClassifier[L, F] (
  C:Double = 1.0,
  eps:Double = 0.01,
  bias:Boolean = false)
  extends LiblinearClassifier[L, F](SolverType.L2R_L2LOSS_SVC, C, eps, bias)

object LiblinearClassifier {
  val logger = LoggerFactory.getLogger(classOf[LiblinearClassifier[String, String]])

  def loadFrom[L, F](fileName:String):LiblinearClassifier[L, F] = {
    val r = new BufferedReader(new FileReader(fileName))
    val c = loadFrom[L, F](r)
    r.close()
    c
  }

  def loadFrom[L, F](r:Reader):LiblinearClassifier[L, F] = {
    val reader = Files.toBufferedReader(r)
    val fl = Lexicon.loadFrom[F](reader)
    val ll = Lexicon.loadFrom[L](reader)
    val bits = reader.readLine().split("\\s+")
    val bias = bits(0).toBoolean
    val biasFeatureIndex = bits(1).toInt
    val c = new LiblinearClassifier[L, F](SolverType.L2R_LR, 1.0, 0.01, bias) // only bias matters at prediction time
    c.biasFeatureIndex = biasFeatureIndex
    c.featureLexicon = Some(fl)
    c.labelLexicon = Some(ll)
    c.model = Linear.loadModel(reader)
    c
  }
}
