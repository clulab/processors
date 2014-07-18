package edu.arizona.sista.learning

import edu.arizona.sista.struct.Counter
import edu.arizona.sista.struct.Lexicon
import weka.core._
import java.util
import scala.collection.mutable
import scala.Option
import scala.collection.mutable.ListBuffer
import hr.irb.fastRandomForest.FastRandomForest
import org.slf4j.LoggerFactory
import RandomForestClassifier.logger
import java.io._

/**
 * Wrapper for the fast-random-forest classifier
 * User: mihais
 * Date: 12/6/13
 */
class RandomForestClassifier[L, F]( val numTrees:Int = 1000,
                                    val featureSampleRatio:Double = -1.0,
                                    val maxTreeDepth:Int = 0,
                                    val numThreads:Int = 0,
                                    val randomSeed:Int = 1 ) extends Classifier[L, F] with Serializable {
  /** List of all attributes (features), excluding the class label */
  private var attributeIndex = new mutable.HashMap[String, Attribute]
  /** The class attribute */
  private var classAttribute:Option[Attribute] = None
  /** Used to convert labels from Weka's strings to L */
  private var labelMap = new mutable.HashMap[String, L]()

  private var instances:Option[Instances] = None
  private var classifier:Option[FastRandomForest] = None

  override def classOf(d:Datum[L, F]): L = {
    val scores = getScores(d)
    var max = Double.MinValue
    var bestIndex = -1
    for(i <- 0 until scores.length) {
      if(scores(i) > max) {
        max = scores(i)
        bestIndex = i
      }
    }
    assert(bestIndex >= 0)
    labelMap.get(classAttribute.get.value(bestIndex)).get
  }

  override def scoresOf(d:Datum[L, F]): Counter[L] = {
    val scores = getScores(d)
    val c = new Counter[L]
    for(i <- 0 until scores.length) {
      val l = labelMap.get(classAttribute.get.value(i)).get
      c.incrementCount(l, scores(i))
    }
    c
  }

  private def getScores(d:Datum[L, F]): Array[Double] = {
    val i = mkInstanceFromDatum(d, attributeIndex.size + 1)
    classifier.get.distributionForInstance(i)
  }

  override def train(dataset:Dataset[L, F], indices:Array[Int]) {
    // create the label map
    for(l <- dataset.labelLexicon.keySet)
      labelMap += asString(l) -> l

    // create the Weka dataset
    instances = Some(datasetToInstances(dataset, indices))

    // create and configure the random forest
    val rfc = new FastRandomForest
    rfc.setNumTrees(numTrees)

    if(featureSampleRatio > 1)
      throw new RuntimeException("ERROR: featureSampleRatio must be <= 1!")
    else if(featureSampleRatio > 0) // a ratio of all features
      rfc.setNumFeatures((featureSampleRatio * attributeIndex.size).toInt)
    if(featureSampleRatio == 0) // use all features
      rfc.setNumFeatures(attributeIndex.size)
    else // use a abs(ratio) * sqrt(total features)
      rfc.setNumFeatures(
        math.min(
          (math.abs(featureSampleRatio) * math.sqrt(attributeIndex.size)).toInt,
          attributeIndex.size))

    rfc.setMaxDepth(maxTreeDepth)
    rfc.setSeed(randomSeed)
    rfc.setNumThreads(numThreads)

    // train
    logger.debug("Training with the following parameters:")
    logger.debug("Number of decision trees: " + rfc.getNumTrees)
    logger.debug("Max depth of decision trees: " + rfc.getMaxDepth)
    logger.debug("Total features and features used for each random split: " + attributeIndex.size + " " + rfc.getNumFeatures)
    logger.debug("Random seed: " + rfc.getSeed)
    rfc.buildClassifier(instances.get)
    classifier = Some(rfc)
    logger.debug("Completed training.")

    // instances are no longer needed after training
    instances.get.delete()
  }

  private def asString[T](v:T):String = {
    v match {
      case s: String => s
      case _ => v.toString
    }
  }

  private def datasetToInstances(dataset:Dataset[L, F], indices:Array[Int]):Instances = {
    val featureLexicon = dataset.featureLexicon
    val labelLexicon = dataset.labelLexicon

    //
    // this will store all Weka attributes, including the class attribute
    //
    val attributes = new util.ArrayList[Attribute]()
    var index = 0
    // create numeric attributes for all features
    for(k <- featureLexicon.keySet) {
      val ks = asString(k)
      val a = new Attribute(ks, index)
      attributes.add(a)
      attributeIndex += ks -> a
      index += 1
    }
    // create the class attribute as the last one
    val labels = new util.ArrayList[String]()
    for(l <- labelLexicon.keySet) {
      val ls = asString(l)
      labels.add(ls)
    }
    classAttribute = Some(new Attribute("theClass", labels, index))
    attributes.add(classAttribute.get)
    index += 1

    //
    // create instances
    //
    val instances = new Instances("Dataset", attributes, indices.length)
    for(i <- 0 until indices.length) {
      val label = dataset.labels(indices(i))
      val fs = dataset.featuresCounter(indices(i))
      val inst = mkInstance(featureLexicon, labelLexicon, label, fs, index)
      inst.setDataset(instances)
      instances.add(inst)
    }

    // set the index of the class attribute
    instances.setClassIndex(classAttribute.get.index())

    instances
  }

  /** Creates a sparse instance from one datum */
  private def mkInstanceFromDatum(d:Datum[L, F], maxNumAttributes:Int, instanceWeight:Double = 1.0):Instance = {
    // collect all non-zero features
    val featIndicesAndValues = new ListBuffer[(Int, Double)]()
    val features = d.featuresCounter
    for(f <- features.keySet) {
      val fa = attributeIndex.get(asString(f))
      // the feature might not have been seen in training
      if(fa.isDefined) {
        val attValue = features.getCount(f)
        if(attValue != 0.0) {
          featIndicesAndValues += new Tuple2(fa.get.index(), attValue)
        }
      }
    }

    // create the instance
    val (indices, attValues) = mkSparseArrays(featIndicesAndValues)
    val i = new SparseInstance(instanceWeight, attValues, indices, maxNumAttributes)
    i.setDataset(instances.get)

    i
  }

  /** Creates a sparse instance from one row in the dataset */
  private def mkInstance(
                          featureLexicon:Lexicon[F],
                          labelLexicon:Lexicon[L],
                          label:Int,
                          features:Counter[Int],
                          maxNumAttributes:Int,
                          instanceWeight:Double = 1.0):Instance = {
    // collect all non-zero features
    val featIndicesAndValues = new ListBuffer[(Int, Double)]()
    for(f <- features.keySet) {
      val attValue = features.getCount(f)
      if(attValue != 0.0) {
        val k = asString(featureLexicon.get(f))
        val attIndex = attributeIndex.get(k).get.index()
        featIndicesAndValues += new Tuple2(attIndex, attValue)
      }
    }

    // add the class attribute
    val ls = labelLexicon.get(label)
    val classValue = classAttribute.get.indexOfValue(asString(ls))
    if(classValue == -1) throw new RuntimeException(s"ERROR: unknown label $label:$ls")
    featIndicesAndValues += new Tuple2(classAttribute.get.index(), classValue)

    // create the instance
    val (indices, attValues) = mkSparseArrays(featIndicesAndValues)
    new SparseInstance(instanceWeight, attValues, indices, maxNumAttributes)
  }

  private def mkSparseArrays(fivs:Iterable[(Int, Double)]):(Array[Int], Array[Double]) = {
    // sort weka features by index
    val sortedFeatures = fivs.toList.sortBy(_._1)

    // create the sparse arrays for feature indices and values
    val indices = new Array[Int](sortedFeatures.size)
    val attValues = new Array[Double](sortedFeatures.size)
    var i = 0
    for(sf <- sortedFeatures) {
      indices(i) = sf._1
      attValues(i) = sf._2
      i += 1
    }

    (indices, attValues)
  }

  /** Saves the current model to a file */
  override def saveTo(writer:Writer) { throw new RuntimeException("ERROR: saving to Writer not supported yet!") }

  override def saveTo(fn:String) {
    val os = new ObjectOutputStream(new FileOutputStream(fn))
    os.writeObject(this)
    os.close()
  }
}

object RandomForestClassifier {
  val logger = LoggerFactory.getLogger(classOf[RandomForestClassifier[String, String]])

  def loadFrom[L, F](fileName:String):RandomForestClassifier[L, F] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[RandomForestClassifier[L, F]]
    is.close()
    c
  }
}
