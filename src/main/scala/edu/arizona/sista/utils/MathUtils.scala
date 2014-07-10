package edu.arizona.sista.utils

import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import java.util.Random

/**
 * Math utility methods useful for stats and ML
 * User: mihais
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

  def logSum(logInputs:Array[Double]):Double =
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

  /**
   * If a difference is bigger than this in log terms, then the sum or
   * difference of them will just be the larger (to 12 or so decimal
   * places for double, and 7 or 8 for float).
   */
  val LogTolerance:Double = 30.0
  val LogToleranceFloat:Float = 20.0f

  def randomize[T](l: Array[T], rand:Random): Array[T] = {
    for (i <- l.length - 1 to 1 by -1) {
      val j = rand.nextInt(i)
      val tmp = l(j)
      l(j) = l(i)
      l(i) = tmp
    }
    l
  }
}
