package org.clulab.learning

import java.io._
import java.util.Properties
import org.slf4j.LoggerFactory

import scala.Serializable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.sys.process._

import org.clulab.struct.{ Counters, Counter, Lexicon }
import org.clulab.utils.StringUtils

import SVMRankingClassifier.logger

/**
 * Wrapper for SVMrank: trains using svm_rank_learn but predicts using native Scala code
 * Only the linear kernel is supported
 * User: mihais
 * Date: 4/23/13
 * Last Modified: Fix compiler issue: import scala.io.Source.
 */
class SVMRankingClassifier[F] (
                                val workingDir:String,
                                val modelFile:String = "model.dat",
                                val trainFile:String = "train.dat",
                                val debugFile:String = "",
                                val testFile:String = "test.dat",
                                val cLight:Double = 0.1,
                                val keepIntermediateFiles:Boolean = false) extends RankingClassifier[F] with Serializable {

  def this(props:Properties) =
    this(
      props.getProperty("workingDir", "."),
      props.getProperty("modelFile", "model.dat"),
      props.getProperty("trainFile", "train.dat"),
      props.getProperty("debugFile", ""),
      props.getProperty("testFile", "test.dat"),
      StringUtils.getDouble(props, "c", 0.1),
      StringUtils.getBool(props, "keepIntermediateFiles", false))

  var featureLexicon:Option[Lexicon[F]] = None
  var weights:Option[Array[Double]] = None
  var weightsOriginal:Option[Array[Double]] = None

  /** Will contain the test datums in svm_rank format, for offline testing */
  var evalFile:Option[PrintWriter] = None
  /** Keeps track of qids for the dump to evalFile */
  var qid:Int = 0

  def train(dataset:RankingDataset[F], spans:Option[Iterable[(Int, Int)]] = None) {
    val trainPath = workingDir + File.separator + trainFile
    val trainWriter = new PrintWriter(trainPath)
    val n = mkTrainFile(trainWriter, dataset, spans)
    trainWriter.close()
    logger.debug("Created training file: " + trainPath)

    val cRank = cLight * n
    val modelPath = workingDir + File.separator + modelFile
    val cmd =
      "svm_rank_learn " +
        "-c " + cRank + " " +
        "-# 2000 " +
        "-e 0.001 " +
        //      "-w 1 " +
        trainPath + " " + modelPath
    logger.debug("Running TRAIN command: " + cmd)
    val exitCode = cmd.!
    logger.debug("svm_rank_learn terminated with exit code " + exitCode)
    if(exitCode != 0) throw new RuntimeException("ERROR: svm_rank_learn terminated with exit code " + exitCode + "!")

    featureLexicon = Some(Lexicon(dataset.featureLexicon))
    weights = Some(loadModelWeights(modelPath))

    weightsOriginal = Some(loadModelWeights(modelPath))       // for dynamic thresholding (Jurafsky paper)

    debug()

    if(! keepIntermediateFiles) {
      new File(trainPath).delete()
      new File(modelPath).delete()
    } else {
      logger.info("TRAINING file saved as: " + trainPath)
      logger.info("MODEL file saved as: " + modelPath)
    }
  }


  def trainWithBagging(dataset:RankingDataset[F], numBags:Int, pw:PrintWriter) {
    val avgWeights = Array.fill[Double](dataset.numFeatures+1)(0.0)

    for (i <- 0 until numBags) {
      // Train one "bag" using a bootstrap resampled dataset
      val bagDataset = dataset.bootstrapSample(dataset.size)
      train(bagDataset)

      // Add model weights to average (lexicon should be identical between bootstrapResampled dataset and original dataset
      val bagWeights = weights.get
      println ("bagWeights: " + bagWeights.size)
      println ("avgWeights: " + avgWeights.size)
      assert (bagWeights.size == avgWeights.size)
      for (i <- 0 until bagWeights.size) {
        avgWeights(i) += (bagWeights(i) / numBags)
      }

      pw.println ("*****************")
      pw.println ("  BAG SAMPLE " + i)
      pw.println ("*****************")
      displayModel(pw)
    }

    weights = Some(avgWeights)
    pw.println ("*****************")
    pw.println ("    AVERAGED ")
    pw.println ("*****************")
    displayModel(pw)

  }

  def displayModel(pw:PrintWriter) {
    val weightSet = weights.get
    val lexicon = featureLexicon.get
    pw.println ("SVM Weights: ")
    println ("weights.size: " + weightSet.size)
    println ("featureLexicon.size " + lexicon.size)
    for (j <- 0 until (weightSet.size - 1)) {
      val featureName = lexicon.get(j)
      println ("weight: " + weightSet(j).formatted("%3.5f") + " \t feature: " + featureName + "  (idx:" + j + ")" )
      pw.println ("weight: " + weightSet(j).formatted("%3.5f") + " \t feature: " + featureName + "  (idx:" + j + ")" )
    }
    pw.println ("- - - - - - - - - - - - - - - - - - - - - - - - - - - - ")
    pw.println ("")
    pw.flush()

  }

  def clipWeights(thresh:Double) {
    val weightsOrig = weightsOriginal.get
    val weightsClipped = new Array[Double](weightsOrig.size)

    for (j <- 0 until weightsOrig.size) {
      val median = weightsOrig(j)
      if (math.abs(median) >= thresh) {
        weightsClipped(j) = median
      } else {
        weightsClipped(j) = 0.0f
      }
    }

    weights = Some(weightsClipped)
  }

  /** Removes features whose weight is lower than the weight of the reference feature * threshold */
  def clipWeightsRelativeToOneFeature(thresh:Double, feature:F) {
    // To keep weights that are at least 0.10 of the IR score, for example.
    val weightsOrig = weightsOriginal.get
    val weightsClipped = new Array[Double](weightsOrig.size)

    // Find value of IR feature (this could be cleaned up)
    var irWeight:Double = 1.0
    for (i <- 0 until (weightsOrig.size-1)) {
      if (featureLexicon.get.get(i) == feature) {
        irWeight = weightsOrig(i)
      }
    }

    logger.debug ("Reference weight: " + irWeight)
    val newThresh = thresh * irWeight

    for (j <- 0 until weightsOrig.size) {
      val median = weightsOrig(j)
      if (math.abs(median) >= newThresh) {
        weightsClipped(j) = median
      } else {
        weightsClipped(j) = 0.0f
      }
    }

    weights = Some(weightsClipped)
  }

  def loadModelWeights(modelPath:String):Array[Double] = {
    var modelLine:Option[String] = None
    var numFeats:Int = 0
    for(line <- Source.fromFile(modelPath).getLines()) {
      val (content, comment) = splitSVMLine(line)
      if(comment.contains("kernel type") &&
        content.toInt != 0) {
        throw new RuntimeException("ERROR: only linear kernels are currently supported!")
      } else if(comment.contains("highest feature index")) {
        numFeats = content.toInt
      } else if(comment.contains("number of support vectors plus 1") &&
        content.toInt != 2) {
        throw new RuntimeException("ERROR: only linear kernels with a single SV are currently supported!")
      } else if(comment.contains("threshold b") &&
        content.toInt != 0) {
        throw new RuntimeException("ERROR: threshold b must be 0!")
      } else {
        modelLine = Some(content)
      }
    }
    if(numFeats == 0) {
      throw new RuntimeException("ERROR: cannot find the number of features!")
    }
    if(modelLine == None) {
      throw new RuntimeException("ERROR: cannot find model weights!")
    }

    val bits = modelLine.get.split("\\s+")
    if(bits(0).toInt != 1) {
      throw new RuntimeException("ERROR: first value in weight line must be 1!")
    }

    val weights = new Array[Double](numFeats)
    for(i <- 0 until weights.length) weights(i) = 0.0
    for(i <- 1 until bits.length) {
      val feat = bits(i).split(":")
      // our feature ids start at 0, but svm_rank requires features to start at 1
      weights(feat(0).toInt - 1) = feat(1).toDouble
    }
    weights
  }

  private def splitSVMLine(line:String):(String, String) = {
    val pound = line.indexOf("#")
    if(pound < 0) return (line, "")
    (line.substring(0, pound).trim, line.substring(pound + 1). trim)
  }

  private def mkFullFold(size:Int): Iterable[(Int, Int)] = {
    val folds = new Array[(Int, Int)](1)
    folds(0) = new Tuple2(0, size)
    folds
  }

  def mkTrainFile(pw:PrintWriter, d:RankingDataset[F], spans:Option[Iterable[(Int, Int)]]):Int = {
    var n = 0
    val trainFolds = spans.getOrElse(mkFullFold(d.size))
    for(fold <- trainFolds) {
      for(i <- fold._1 until fold._2) {
        val qid = i + 1
        val queryLabels = d.labels(i)

        pw.println("# queryOffset " + qid)
        for(j <- 0 until d.querySize(i)) {
          val l = queryLabels(j)
          val fs = d.featuresCounter(i, j)
          val fids = fs.keySet.toList.sorted

          pw.print(l + " qid:" + qid)
          // our feature ids start at 0, but svm_rank requires features to start at 1
          fids.foreach(fid => pw.print(" " + (fid + 1) + ":" + fs.getCount(fid)))
          pw.println()
        }
        n += 1
      }
    }
    n
  }

  def mkTestFile(pw:PrintWriter, ds:Iterable[Datum[Int, F]], qid:Int) = {
    for(d <- ds) {
      val l = d.label
      val fs = mkDatumVector(d)
      val fids = fs.keySet.toList.sorted

      pw.print(l + " qid:" + qid)
      // our feature ids start at 0, but svm_rank requires features to start at 1
      fids.foreach(fid => pw.print(" " + (fid + 1) + ":" + fs.getCount(fid)))
      pw.println()
    }
  }

  def mkDatumVector(datum:Datum[Int, F]):Counter[Int] = {
    val c = new Counter[Int]
    val fc = datum.featuresCounter
    for(f <- fc.keySet) {
      val idx = featureLexicon.get.get(f)
      if (!idx.isEmpty) {                         // For queries with features that haven't been seen before, we ignore those unseen features
        c.setCount(idx.get, fc.getCount(f))
      }
    }
    c
  }

  /** Opens the evaluation file, which contains datums in svm_rank format, for offline testing */
  def openEvalFile() {
    if(testFile == "") throw new RuntimeException("ERROR: testFile unspecified!")
    evalFile = Some(new PrintWriter(testFile))
  }
  /** Closes the evaluation file; evaluation is complete */
  def closeEvalFile() {
    evalFile.foreach(_.close())
    logger.info("TESTING file saved as: " + testFile)
  }
  /** Increments the qid; for the offline evaluation */
  def setQid(qid:Int) {
    this.qid = qid
  }

  def scoresOf(queryDatums:Iterable[Datum[Int, F]]):Iterable[Double] = {
    if(! evalFile.isEmpty) {
      mkTestFile(evalFile.get, queryDatums, qid)
    }

    if(weights.isEmpty) {
      throw new RuntimeException("ERROR: cannot call scoresOf without model weights!")
    }
    if(featureLexicon.isEmpty) {
      throw new RuntimeException("ERROR: cannot call scoresOf without a feature lexicon!")
    }
    val scores = new ArrayBuffer[Double]

    for(datum <- queryDatums) {
      val datumVector = mkDatumVector(datum)

      //## Debug
      //      val w = weights.get
      //      println ("scoresOf: weights = " + w.toList)
      //      println ("scoresOf: datumVector = " + datumVector)

      val score = Counters.dotProduct(weights.get, datumVector)
      // TEST###
      //      println ("scoresOf: score: = " + score)
      //val score = Counters.dotProductOnlyPositive(weights.get, datumVector)
      scores += score
    }
    scores.toArray
  }

  def saveTo(fileName:String) {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    os.writeObject(this)
    os.close()
  }

  /** Saves important info to this file for debug purposes */
  def debug() {
    if(debugFile.length == 0) return
    var features = new ArrayBuffer[(String, Int, Double)]

    val pw = new PrintWriter(debugFile)
    for(f <- featureLexicon.get.keySet)  {
      val idx = featureLexicon.get.get(f)
      idx match {
        case Some(x) => if (x < weights.get.size) { features.append ( (f.toString, featureLexicon.get.get(f).getOrElse(-1), weights.get(x)) ) }
        case _ =>
      }
    }

    // Sort features
    features = features.sortBy(- _._3)

    // Output features
    for (i <- 0 until features.size) {
      val feature = features(i)
      var featureString = feature._1
      for (j <- 0 until (20 - featureString.size)) featureString += " "       // Make featureString a constant length for formatting
      pw.println (featureString + " \t weight: " + feature._3)
    }

    pw.println ("")
    pw.println("Weights:")
    var first = true
    for(i <- 0 until weights.get.size) {
      if(weights.get(i) != 0.0) {
        if(! first) pw.print(" ")
        pw.print(i + ":" + weights.get(i))
        first = false
      }
    }
    pw.println()
    pw.close()
  }
}

object SVMRankingClassifier {
  val logger = LoggerFactory.getLogger(classOf[SVMRankingClassifier[String]])

  def loadFrom[F](fileName:String):SVMRankingClassifier[F] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[SVMRankingClassifier[F]]
    is.close()
    c
  }
}
