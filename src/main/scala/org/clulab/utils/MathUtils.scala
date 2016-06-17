package org.clulab.utils

import scala.collection.mutable.{ListBuffer, ArrayBuffer}

import scala.util

/**
 * Math utility methods useful for stats and ML
 * User: mihais, dfried
 * Date: 4/23/13
 */
object MathUtils {

  /**
   * Puts a softmax layer over a collection of scores, so they look like probabilities
   * @param scores A collection of unnormalized scores
   * @param gamma Indicates how spiked the probability distribution should be
   * @return
   */
  def softmax(scores:Iterable[Double], gamma:Double = 1.0):List[Double] = {
    val scoreArrayBuffer = new ArrayBuffer[Double]
    for(s <- scores) scoreArrayBuffer += s * gamma
    val scoreArray = scoreArrayBuffer.toArray
    val softmaxes = new ListBuffer[Double]

    val logSumStatic = logSum(scoreArray)
    for(s <- scores) {
      val logSoftmax = (gamma * s) - logSumStatic
      softmaxes += math.exp(logSoftmax)
    }

    softmaxes.toList
  }

  /**
   * Puts a softmax layer over a collection of scores, so they look like probabilities
   * @param scores A collection of unnormalized scores
   * @param gamma Indicates how spiked the probability distribution should be
   * @return
   */
  def softmaxFloat(scores:Iterable[Float], gamma:Float = 1.0f):List[Float] = {
    val scoreArrayBuffer = new ArrayBuffer[Float]
    for(s <- scores) scoreArrayBuffer += s * gamma
    val scoreArray = scoreArrayBuffer.toArray
    val softmaxes = new ListBuffer[Float]

    val logSumStatic = logSum(scoreArray)
    for(s <- scores) {
      val logSoftmax = (gamma * s) - logSumStatic
      softmaxes += math.exp(logSoftmax).toFloat
    }

    softmaxes.toList
  }

  /**
   * Puts a softmax layer over a collection of scores, so they look like probabilities
   * @param vector A collection of unnormalized scores
   * @param gamma Indicates how spiked the probability distribution should be
   * @return
   */
  def denseSoftmax(vector: Array[Double], gamma: Double = 1.0): Array[Double] = {
    val scoreArray = if (gamma != 1.0) for{
      v <- vector
    } yield gamma * v
    else vector

    val logSumStatic = logSum(scoreArray)
    for {
      s <- vector
    } yield math.exp((gamma * s) - logSumStatic)
  }

  /**
   * Puts a softmax layer over a collection of scores, so they look like probabilities
   * @param vector A collection of unnormalized scores
   * @param gamma Indicates how spiked the probability distribution should be
   * @return
   */
  def denseSoftmaxFloat(vector: Array[Float], gamma: Float = 1.0f): Array[Float] = {
    val scoreArray = if (gamma != 1.0f) for{
      v <- vector
    } yield gamma * v
    else vector

    val logSumStatic = logSum(scoreArray)
    for {
      s <- vector
    } yield math.exp((gamma * s) - logSumStatic).toFloat
  }

  def logSum(logInputs:Array[Double]):Double =
    logSum(logInputs, 0, logInputs.length)

  def logSum(logInputs:Array[Float]):Float =
    logSum(logInputs, 0, logInputs.length)

  /**
   * Returns the log of the portion between <code>fromIndex</code>, inclusive, and
   * <code>toIndex</code>, exclusive, of an array of numbers, which are
   * themselves input in log form.  This is all natural logarithms.
   * Reasonable care is taken to do this as efficiently as possible
   * (under the assumption that the numbers might differ greatly in
   * magnitude), with high accuracy, and without numerical overflow.  Throws an
   * IllegalArgumentException if <code>logInputs</code> is of length zero.
   * Otherwise, returns Double.NegativeInfinity if <code>fromIndex</code> &gt;=
   * <code>toIndex</code>.
   * @param logInputs Numbers in log form
   * @param fromIndex Start offset (inclusive)
   * @param toIndex End offset (exclusive)
   * @return log(x1 + ... + xn)
   */
  def logSum(logInputs:Array[Double], fromIndex:Int, toIndex:Int):Double = {
    if (logInputs.length == 0)
      throw new IllegalArgumentException()

    if(fromIndex >= 0 && toIndex < logInputs.length && fromIndex >= toIndex)
      return Double.NegativeInfinity

    var maxIdx = fromIndex
    var max = logInputs(fromIndex)
    for (i <- fromIndex + 1 until toIndex) {
      if (logInputs(i) > max) {
        maxIdx = i
        max = logInputs(i)
      }
    }

    var haveTerms = false
    var intermediate = 0.0
    var cutoff = max - LogTolerance
    for (i <- fromIndex until toIndex) {
      if (i != maxIdx && logInputs(i) > cutoff) {
        haveTerms = true
        intermediate += math.exp(logInputs(i) - max)
      }
    }
    if (haveTerms) {
      return max + math.log(1.0 + intermediate)
    }
    max
  }

  def logSum(logInputs:Array[Float], fromIndex:Int, toIndex:Int):Float = {
    if (logInputs.length == 0)
      throw new IllegalArgumentException()

    if(fromIndex >= 0 && toIndex < logInputs.length && fromIndex >= toIndex)
      return Float.NegativeInfinity

    var maxIdx = fromIndex
    var max = logInputs(fromIndex)
    for (i <- fromIndex + 1 until toIndex) {
      if (logInputs(i) > max) {
        maxIdx = i
        max = logInputs(i)
      }
    }

    var haveTerms = false
    var intermediate = 0.0f
    var cutoff = max - LogTolerance
    for (i <- fromIndex until toIndex) {
      if (i != maxIdx && logInputs(i) > cutoff) {
        haveTerms = true
        intermediate += math.exp(logInputs(i) - max).toFloat
      }
    }
    if (haveTerms) {
      return max + math.log(1.0 + intermediate).toFloat
    }
    max
  }

  /**
   * If a difference is bigger than this in log terms, then the sum or
   * difference of them will just be the larger (to 12 or so decimal
   * places for double, and 7 or 8 for float).
   */
  val LogTolerance:Double = 30.0
  val LogToleranceFloat:Float = 20.0f

  def randomize[T](l: Array[T], rand:util.Random): Array[T] = {
    for (i <- l.length - 1 to 1 by -1) {
      val j = rand.nextInt(i)
      val tmp = l(j)
      l(j) = l(i)
      l(i) = tmp
    }
    l
  }

  // Manifest: http://stackoverflow.com/questions/6085085/why-cant-i-create-an-array-of-generic-type
  def nBest[T: Manifest](scoringFunction: T=>Double)(xs: Iterable[T], howMany:Int): List[
    (T, Double)] = {
    val bestd = new Array[Double](howMany)
    val bestw = new Array[T](howMany)
    var i = 0
    while(i < howMany) {
      bestd(i) = Double.MinValue
      i += 1
    }
    for(x <- xs) {
      val score = scoringFunction(x)
      if(score > bestd(howMany - 1)) {
        i = 0
        var found = false
        while(i < howMany && ! found) {
          if(score > bestd(i)) {
            var j = howMany - 1
            while(j > i) {
              bestd(j) = bestd(j - 1)
              bestw(j) = bestw(j - 1)
              j -= 1
            }
            bestd(i) = score
            bestw(i) = x
            found = true
          }
          i += 1
        }
      }
    }
    val l = (bestw zip bestd).toList
    if (xs.size < l.size) l.take(xs.size) else l
  }

  /** sample howMany elements uniformly from xs. Doesn't retain order of xs */
  def sampleStream[T: Manifest](xs: Iterable[T], howMany: Int) =
    nBest((x:T) => scala.util.Random.nextDouble)(xs, howMany).map(_._1)

  def histogram(xs: Traversable[Double], boundaries: Seq[Double]): Map[(Double, Double), Int] = {
    // make sure divisions is monotonically increasing
    require((boundaries, boundaries.tail).zipped.forall(_ < _), "boundaries must be an ascending sequence")
    // TODO: do a binary search if we have lots of buckets?
    def getBucket(x: Double) = {
      boundaries.zipWithIndex.find({ case (b, i) => b >= x }).map(_._2).getOrElse(boundaries.size)
    }
    val extendedBounds = xs.min +: boundaries :+ xs.max
    def getRange(bucket: Int) = {
      val zipped = extendedBounds.zip(extendedBounds.tail)
      zipped(bucket)
    }
    val countsByBucket = xs.groupBy(getBucket).mapValues(_.size).toMap
    (for {
      (bucket) <- 0 to boundaries.size
    } yield (getRange(bucket) -> countsByBucket.getOrElse(bucket, 0))).toMap
  }
  
  def histogram(xs: Traversable[Double], numBuckets: Int): Map[(Double, Double), Int] = {
    val (min, max) = (xs.min, xs.max)
    val step = (max - min) / numBuckets
    require(step != 0, "xs has all same values")
    val divisions = (min + step) to (max - step / 2) by step
    histogram(xs, divisions)
  }
}
