package org.clulab.utils

import scala.language.postfixOps

/**
 * Created by dfried on 5/22/14
 */
object EvaluationStatistics {

  case class Table(var tp: Int, var fp: Int, var tn: Int, var fn: Int) {
    def accuracy = (tp + tn).toDouble / (tp + fp + tn + fn)

    def precision = tp.toDouble / (tp + fp)

    def recall = tp.toDouble / (tp + fn)

    def f1 = 2 * precision * recall / (precision + recall)

    def total = tp + fp + tn + fn

    def trueCount = tp + fn

    def falseCount = fp + tn

    def positiveCount = tp + fp

    def negativeCount = tn + fn

    def + (other: Table) = Table(tp + other.tp, fp + other.fp, tn + other.tn, fn + other.fn)
  }

  def makeTables[A](outcome: A)(predicted: Seq[A], actual: Seq[A]): Table = {
    require(predicted.size == actual.size, "predicted and actual labels must be same size")
    val counts = Table(0, 0, 0, 0)
    for ((p, a) <- predicted zip actual) {
      if (p == outcome) { // positive
        if (a == outcome)  counts.tp += 1
        else counts.fp += 1
      } else { // negative
        if (a == outcome) counts.fn += 1
        else counts.tn += 1
      }
    }
    counts
  }

  def makeTables[A](predicted: Seq[A], actual: Seq[A]): Map[A, Table] = {
    val vals: Set[A] = (predicted ++ actual).toSet
    (for {
      v <- vals.toSeq
    } yield v -> makeTables(v)(predicted, actual)).toMap
  }

  def microAverage[A](tables: Map[A, Table])(accessor: Table => Double) = {
    accessor(tables.values.reduce(_ + _))
  }

  def macroAverage[A](tables: Map[A, Table])(accessor: Table => Double): Double = {
    val N_classes = tables.size
    tables.values.map(accessor).sum / N_classes
  }

  def weightedAverage[A](tables: Map[A, Table])(accessor: Table => Double): Double = {
    val trueCounts: Map[A, Int] = tables.mapValues(_.trueCount)
    val N_data = tables.values.head.total
    tables.map({
      case (a, table) => accessor(table) * trueCounts(a).toDouble / N_data
    }).sum
  }

  /**
   * Calculate significance of the given evaluation statistic for labels predicted by a system over labels predicted by
   * a baseline, compared to actual labels, using the bootstrap.
   * @param stat A function of EvaluationStatistics, such as accuracy or microF1
   * @param predicted The labels predicted by the treatment system
   * @param baseline The labels predicted by the baseline (control) system
   * @param actual The actual labels
   * @param N_samples Number of samples to use in bootstrap
   * @tparam A The label type
   * @return The p-value of the statistic
   */
  def classificationSignificance[A](stat: EvaluationStatistics[A] => Double)(predicted: Seq[A], baseline: Seq[A],
                                                                     actual: Seq[A], N_samples: Int = 10000) = {
    require(predicted.size == baseline.size && baseline.size == actual.size, "label arrays must be same size")
    val N_labels = predicted.size

    // create an array of the labels zipped for easy access in bootstrap resampling
    val zipped = new Array[(A, A, A)](predicted.size)
    for (i <- 0 until N_labels)
      zipped(i) = (predicted(i), baseline(i), actual(i))

    def bootstrapDifference = {
      // generate a length N_labels vector of indices for sampling w/ replacement
      val indices = (0 until N_labels).map(_ => util.Random.nextInt(N_labels))
      // get the corresponding labels from each array
      val (predSampled, baseSampled, actSampled) = indices map (zipped(_)) unzip3
      // calculate the evaluation statistic, stat for predicted vs actual and for baseline vs actual
      val predictedEval = new EvaluationStatistics[A](predSampled, actSampled)
      val baselineEval = new EvaluationStatistics[A](baseSampled, actSampled)
      // return the difference of the stat
      stat(predictedEval) - stat(baselineEval)
    }

    // produce N_samples samples from the sequences, and compute the difference in stat between the
    // predicted and baseline accuracies (compared to actual labels) for each sample. Sort by value, increasing, and
    // find the index of the difference corresponding to the null hypothesis
    val sampledDifferences: Array[Double] = (for {
      sampleIx <- (0 until N_samples).toArray
    } yield bootstrapDifference).sorted
    val indexOfZero = {
      val i = java.util.Arrays.binarySearch(sampledDifferences, 0.0)
      if (i >= 0) i else -(i + 1)
    }
    // return the percentile
    indexOfZero.toDouble / N_samples
  }

  /**
   * Calculate accuracy significance for labels predicted by a system over labels
   * predicted by a baseline, compared to actual labels, using the bootstrap.
   * @param predicted The labels predicted by the treatment system
   * @param baseline The labels predicted by the baseline (control) system
   * @param actual The actual labels
   * @param N_samples Number of samples to use in bootstrap
   * @tparam A The label type
   * @return The p-value significance of the accuracy statistic
   */
  def classificationAccuracySignificance[A](predicted: Seq[A], baseline: Seq[A], actual: Seq[A],  N_samples: Int = 10000) =
    classificationSignificance[A](_.accuracy)(predicted, baseline, actual, N_samples)
}

class EvaluationStatistics[A](tables: Map[A, EvaluationStatistics.Table]) {
  import EvaluationStatistics.Table
  def this(predicted: Seq[A], actual: Seq[A]) =
    this(EvaluationStatistics.makeTables(predicted, actual))

  private def microAverage: ((Table) => Double) => Double = EvaluationStatistics.microAverage(tables)

  private def macroAverage: ((Table) => Double) => Double = EvaluationStatistics.macroAverage(tables)

  private def weightedAverage: ((Table) => Double) => Double = EvaluationStatistics.weightedAverage(tables)

  lazy val microPrecision = microAverage(_.precision)

  lazy val microRecall = microAverage(_.recall)

  lazy val microF1 = microAverage(_.f1)

  lazy val macroPrecision = macroAverage(_.precision)

  lazy val macroRecall = macroAverage(_.recall)

  lazy val macroF1 = macroAverage(_.f1)

  lazy val accuracy = tables.values.map(_.tp).sum.toDouble / tables.values.head.total

  /** format of this is based on David Hall's Nak */
  override def toString = {
    def f(d: Double) = "%.4f".format(d)
    val tableRep = tables.map( { case (a, table) => {
      s"$a:\tPrecision: ${f(table.precision)}\tRecall: ${f(table.recall)}\tF1: ${f(table.f1)}\tAccuracy: ${f(table.accuracy)}"
    }}).mkString("\n")
    s"""Evaluation Statistics:
==========
Accuracy: ${f(accuracy)}
Macro\t Precision: ${f(macroPrecision)}\tRecall: ${f(macroRecall)}\tF1: ${f(macroF1)}
Micro\t Precision: ${f(microPrecision)}\tRecall: ${f(microRecall)}\tF1: ${f(microF1)}
==========
${tableRep}"""
  }
}
