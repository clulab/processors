package edu.arizona.sista.learning

import java.io._
import java.util.Properties
import edu.arizona.sista.utils.StringUtils
import org.slf4j.LoggerFactory
import scala.sys.process._
import SVMKRankingClassifier.logger
import scala.Serializable
import java.util
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import SVMKRankingClassifier.DEFAULT_C
import edu.arizona.sista.struct.Lexicon
import edu.arizona.sista.struct.Counter

/**
 * Wrapper for SVM-LIGHT-TK, using the cmd line
 * This is not efficient! It uses the cmd line during evaluation, which is VERY slow.
 * User: mihais
 * Date: 8/14/13
 */
class SVMKRankingClassifier[F](val workingDir:String,
                               val modelFile:String = "model",
                               val trainFile:String = "train",
                               val debugFile:String = "",
                               val cLight:Double = DEFAULT_C,
                               val keepIntermediateFiles:Boolean = false)
  extends RankingClassifier[F] with Serializable {

  /** Content of the svm-light model file */
  var model:String = null
  /** Map from F features to Int */
  var featureLexicon:Lexicon[F] = null

  def this(props:Properties) =
    this(
      props.getProperty("workingDir", "."),
      props.getProperty("modelFile", "model"),
      props.getProperty("trainFile", "train"),
      props.getProperty("debugFile", ""),
      StringUtils.getDouble(props, "c", DEFAULT_C),
      StringUtils.getBool(props, "keepIntermediateFiles", false))

  def train(dataset: RankingDataset[F], spans: Option[Iterable[(Int, Int)]]) {
    val trainPath = File.createTempFile(trainFile, ".tmp", new File(workingDir))
    val modelPath = File.createTempFile(modelFile, ".tmp", new File(workingDir))

    val trainWriter = new PrintWriter(trainPath)
    mkTrainFile(trainWriter, dataset, spans)
    trainWriter.close()
    logger.debug("Created training file: " + trainPath.getAbsolutePath)

    // this is svm_learn from svm-light-TK!
    val cmd = s"svm_learn -t 5 -C + -S 0 -c $cLight " + trainPath.getAbsolutePath + " " + modelPath.getAbsolutePath
    logger.debug("Running TRAIN command: " + cmd)
    val exitCode = cmd.!
    logger.debug("svm_learn terminated with exit code " + exitCode)
    if(exitCode != 0) throw new RuntimeException("ERROR: svm_learn terminated with exit code " + exitCode + "!")

    model = readModel(modelPath.getAbsolutePath)
    featureLexicon = Lexicon(dataset.featureLexicon)

    if(! keepIntermediateFiles) {
      trainPath.delete()
      modelPath.delete()
    }

    val os = new StringWriter()
    val pw = new PrintWriter(os)
    displayModel(pw)
    logger.debug(os.toString)
    pw.close()
  }

  private def readModel(modelPath:String):String = {
    val is = new InputStreamReader(
      new FileInputStream(modelPath),
      "UTF-8")
    val sb = new StringBuilder()
    val chars = new Array[Char](SVMKRankingClassifier.BUFFER_SIZE)
    var n = 0
    while(n != -1) {
      n = is.read(chars)
      if(n != -1) {
        sb.append(new String(util.Arrays.copyOfRange(chars, 0, n)))
      }
    }
    is.close()
    sb.toString()
  }
  private def writeModel(modelTempFile:File) {
    val os = new PrintStream(new FileOutputStream(modelTempFile))
    os.print(model)
    os.close()
  }

  /**
   * Creates the training file for SVM-LIGHT-TK
   * This currently works only as a classifier: +1 good answers, -1 bad answers
   * This is sufficient for StackOverflow and YA, but would not work for Bio
   * @param pw
   * @param d
   * @param spans
   * @return
   */
  def mkTrainFile(pw:PrintWriter, d:RankingDataset[F], spans:Option[Iterable[(Int, Int)]]):Int = {
    d match {
      case dk:RVFKRankingDataset[F] => {
        var n = 0
        var trainFolds = spans.getOrElse(mkFullFold(d.size))
        for(fold <- trainFolds) {
          for(i <- fold._1 until fold._2) {
            val qid = i + 1
            val queryLabels = dk.labels(i)
            val kernels = dk.kernels(i)

            // Generate data points by treating the task as classification: +1 good answer, -1 bad answer
            for (j1 <- 0 until dk.querySize(i)) {
              val l1 = queryLabels(j1)
              val fs1 = dk.featuresCounter(i, j1)
              val k1 = kernels(j1)
              val label = if(l1 > 1) +1 else -1
              saveDatum(pw, label, fs1, k1)
            }
            n += 1
          }
        }
        n
      }
      case _ => {
        throw new RuntimeException("ERROR: cannot use SVMKRankingClassifier with a dataset that is not RVFKRankingDataset!")
      }
    }
  }

  private def saveDatum(pw:PrintWriter, label:Int, features:Counter[Int], kernel:String) {
    // saveDatumLight(pw, label, features)
    saveDatumTk(pw, label, features, kernel)
  }
  private def saveDatumTk(pw:PrintWriter, label:Int, features:Counter[Int], kernel:String) {
    val fids = features.keySet.toList.sorted
    pw.print(s"$label |BT| $kernel |ET|")
    fids.foreach(fid => pw.print(" " + (fid + 1) + ":" + features.getCount(fid)))
    pw.println(" |EV|")
  }
  private def saveDatumLight(pw:PrintWriter, label:Int, features:Counter[Int]) {
    val fids = features.keySet.toList.sorted
    pw.print(s"$label")
    fids.foreach(fid => pw.print(" " + (fid + 1) + ":" + features.getCount(fid)))
    pw.println()
  }

  private def mkDatumVector(fc:Counter[F]):Counter[Int] = {
    val c = new Counter[Int]
    for(f <- fc.keySet) {
      var idx = featureLexicon.get(f)
      if (!idx.isEmpty) {                         // For queries with features that haven't been seen before, we ignore those unseen features
        c.setCount(idx.get, fc.getCount(f))
      }
    }
    c
  }

  private def writeDatums(queryDatums: Iterable[Datum[Int, F]], testTempFile:File) {
    val pw = new PrintWriter(testTempFile)
    for(datum <- queryDatums) {
      datum match {
        case dk:RVFKDatum[Int, F] => {
          val features = mkDatumVector(dk.featuresCounter)
          val kernel = dk.kernel
          val label = if(dk.label > 1) +1 else -1
          saveDatum(pw, label, features, kernel)
        }
        case _ => throw new RuntimeException("ERROR: must have RVFKDatums in SVMKRankingClassifier!")
      }
    }
    pw.close()
  }

  private def mkFullFold(size:Int): Iterable[(Int, Int)] = {
    val folds = new Array[(Int, Int)](1)
    folds(0) = new Tuple2(0, size)
    folds
  }

  def displayModel(pw: PrintWriter) {
    pw.println ("SVMK Model with Support Vectors: ")
    pw.println (model)
    pw.println ("- - - - - - - - - - - - - - - - - - - - - - - - - - - - ")
    pw.println ("")
    pw.flush()
  }

  /**
   * Returns scores that can be used for ranking for a group of datums, from the same query
   * These scores do NOT have to be normalized, they are NOT probabilities!
   * @param queryDatums All datums for one query
   * @return
   */
  def scoresOf(queryDatums: Iterable[Datum[Int, F]]): Iterable[Double] = {
    val testTempFile = File.createTempFile("testFile", ".tmp", new File(workingDir))
    val modelTempFile = File.createTempFile("modelFile", ".tmp", new File(workingDir))
    val outputTempFile = File.createTempFile("outputFile", ".tmp", new File(workingDir))

    logger.debug("Using testTempFile = " + testTempFile.getAbsolutePath)
    logger.debug("Using modelTempFile = " + modelTempFile.getAbsolutePath)
    logger.debug("Using outputTempFile = " + outputTempFile.getAbsolutePath)

    writeModel(modelTempFile)
    writeDatums(queryDatums, testTempFile)

    // this is svm_classify from svm-light-TK!
    val cmd = "svm_classify " +
      testTempFile + " " +
      modelTempFile + " " +
      outputTempFile
    logger.debug("Running EVAL command " + cmd)

    val exitCode = cmd.!
    logger.debug("svm_classify terminated with exit code " + exitCode)
    if(exitCode != 0) throw new RuntimeException("ERROR: svm_classify terminated with exit code " + exitCode + "!")

    var scores = new ArrayBuffer[Double]()
    for(l <- Source.fromFile(outputTempFile).getLines()) {
      scores += l.toDouble
    }

    var i = 0
    val sa = scores.toArray
    val all = new ArrayBuffer[Tuple3[Int, Int, Double]]()
    for(d <- queryDatums) {
      val label = if(d.label > 1) 1 else -1
      all += new Tuple3(i, label, sa(i))
      i += 1
    }

    val sorted = all.toList.sortBy(0 - _._3).toArray
    val os = new StringBuilder
    os.append("IRRANK LABEL SCORE\n")
    for(e <- sorted) {
      os.append(e + "\n")
    }
    logger.debug("Original ranks vs. predicted ranks:\n" + os.toString())

    val origLabel = if(queryDatums.head.label > 1) 1 else -1
    val predLabel = sorted.head._2
    if(origLabel != 1 && predLabel == 1) {
      logger.debug("PAT1 improved over orig")
    } else if(origLabel == 1 && predLabel != 1) {
      logger.debug("PAT1 decreased over orig")
    }

    modelTempFile.delete()
    testTempFile.delete()
    outputTempFile.delete()

    scores.toList
  }

  /** Saves the current model to a file */
  def saveTo(fileName: String) {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    os.writeObject(this)
    os.close()
  }

}

object SVMKRankingClassifier {
  val logger = LoggerFactory.getLogger(classOf[SVMRankingClassifier[String]])
  val BUFFER_SIZE = 4096
  val DEFAULT_C = 0.1

  def loadFrom[F](fileName:String):SVMKRankingClassifier[F] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[SVMKRankingClassifier[F]]
    is.close()
    c
  }
}
