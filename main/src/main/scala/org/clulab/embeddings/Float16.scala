// See https://gist.github.com/non/29f8d66036afca402f96
package org.clulab.embeddings

import scala.math.{pow, round, signum}
import scala.util.Random.nextInt
import java.lang.{Float => JFloat}

/**
  * Float16 represents 16-bit floating-point values.
  *
  * This type does not actually support arithmetic directly. The
  * expected use case is to convert to Float to perform any actual
  * arithmetic, then convert back to a Float16 if needed.
  *
  * Binary representation:
  *
  *     sign (1 bit)
  *     |
  *     | exponent (5 bits)
  *     | |
  *     | |     mantissa (10 bits)
  *     | |     |
  *     x xxxxx xxxxxxxxxx
  *
  * Value interpretation (in order of precedence, with _ wild):
  *
  *     0 00000 0000000000  (positive) zero
  *     1 00000 0000000000  negative zero
  *     _ 00000 __________  subnormal number
  *     _ 11111 0000000000  +/- infinity
  *     _ 11111 __________  not-a-number
  *     _ _____ __________  normal number
  *
  * For non-zero exponents, the mantissa has an implied leading 1 bit,
  * so 10 bits of data provide 11 bits of precision for normal numbers.
  */
class Float16(val raw: Short) extends AnyVal { lhs =>

  def isNaN: Boolean = (raw & 0x7fff) > 0x7c00
  def nonNaN: Boolean = (raw & 0x7fff) <= 0x7c00

  /**
    * Returns if this is a zero value (positive or negative).
    */
  def isZero: Boolean = (raw & 0x7fff) == 0

  def nonZero: Boolean = (raw & 0x7fff) != 0

  def isPositiveZero: Boolean = raw == -0x8000
  def isNegativeZero: Boolean = raw == 0

  def isInfinite: Boolean = (raw & 0x7fff) == 0x7c00
  def isPositiveInfinity: Boolean = raw == 0x7c00
  def isNegativeInfinity: Boolean = raw == 0xfc00

  /**
    * Whether this Float16 value is finite or not.
    *
    * For the purposes of this method, infinities and NaNs are
    * considered non-finite. For those values it returns false and for
    * all other values it returns true.
    */
  def isFinite: Boolean = (raw & 0x7c00) != 0x7c00

  /**
    * Return the sign of a Float16 value as a Float.
    *
    * There are five possible return values:
    *
    *  * NaN: the value is Float16.NaN (and has no sign)
    *  * -1F: the value is a non-zero negative number
    *  * -0F: the value is Float16.NegativeZero
    *  *  0F: the value is Float16.Zero
    *  *  1F: the value is a non-zero positive number
    *
    * PositiveInfinity and NegativeInfinity return their expected
    * signs.
    */
  def signum: Float =
    if (raw == -0x8000) 0F
    else if (raw == 0) -0F
    else if ((raw & 0x7fff) > 0x7c00) Float.NaN
    else ((raw >>> 14) & 2) - 1F

  /**
    * Reverse the sign of this Float16 value.
    *
    * This just involves toggling the sign bit with XOR.
    *
    * -Float16.NaN has no meaningful effect.
    * -Float16.Zero returns Float16.NegativeZero.
    */
  def unary_- : Float16 =
    new Float16((raw ^ 0x8000).toShort)

  def +(rhs: Float16): Float16 =
    Float16.fromFloat(lhs.toFloat + rhs.toFloat)
  def -(rhs: Float16): Float16 =
    Float16.fromFloat(lhs.toFloat - rhs.toFloat)
  def *(rhs: Float16): Float16 =
    Float16.fromFloat(lhs.toFloat * rhs.toFloat)
  def /(rhs: Float16): Float16 =
    Float16.fromFloat(lhs.toFloat / rhs.toFloat)
  def **(rhs: Int): Float16 =
    Float16.fromFloat(pow(lhs.toFloat, rhs).toFloat)

  def <(rhs: Float16): Boolean = {
    if (lhs.raw == rhs.raw || lhs.isNaN || rhs.isNaN) return false
    if (lhs.isZero && rhs.isZero) return false
    val ls = (lhs.raw >>> 15) & 1
    val rs = (rhs.raw >>> 15) & 1
    if (ls < rs) return true
    if (ls > rs) return false
    val le = (lhs.raw >>> 10) & 31
    val re = (rhs.raw >>> 10) & 31
    if (le < re) return ls == 1
    if (le > re) return ls == 0
    val lm = lhs.raw & 1023
    val rm = rhs.raw & 1023
    if (ls == 1) lm < rm else rm < lm
  }

  def <=(rhs: Float16): Boolean = {
    if (lhs.isNaN || rhs.isNaN) return false
    if (lhs.isZero && rhs.isZero) return true
    val ls = (lhs.raw >>> 15) & 1
    val rs = (rhs.raw >>> 15) & 1
    if (ls < rs) return true
    if (ls > rs) return false
    val le = (lhs.raw >>> 10) & 31
    val re = (rhs.raw >>> 10) & 31
    if (le < re) return ls == 1
    if (le > re) return ls == 0
    val lm = lhs.raw & 1023
    val rm = rhs.raw & 1023
    if (ls == 1) lm <= rm else rm <= lm
  }

  def >(rhs: Float16): Boolean =
    !(lhs.isNaN || rhs.isNaN || lhs <= rhs)

  def >=(rhs: Float16): Boolean =
    !(lhs.isNaN || rhs.isNaN || lhs < rhs)

  def ==(rhs: Float16): Boolean =
    if (lhs.isNaN || rhs.isNaN) false
    else if (lhs.isZero && rhs.isZero) true
    else lhs.raw == rhs.raw

  /**
    * Convert this Float16 value to the nearest Float.
    *
    * Non-finite values and zero values will be mapped to the
    * corresponding Float value.
    *
    * All other finite values will be handled depending on whether they
    * are normal or subnormal. The relevant formulas are:
    *
    * * normal:    (sign*2-1) * 2^(exponent-15) * (1 + mantissa/1024)
    * * subnormal: (sign*2-1) * 2^-14 * (mantissa/1024)
    *
    * Given any (x: Float16), Float16.fromFloat(x.toFloat) = x
    *
    * The reverse is not necessarily true, since there are many Float
    * values which are not precisely representable as Float16 values.
    */
  def toFloat: Float = {
    val s = (raw >>> 14) & 2  // sign*2
    val e = (raw >>> 10) & 31 // exponent
    val m = (raw & 1023)      // mantissa
    if (e == 0) {
      // either zero or a subnormal number
      if (m != 0) (s - 1F) * pow(2F, -14).toFloat * (m / 1024F)
      else if (s == 0) -0F
      else 0F
    } else if (e != 31) {
      // normal number
      (s - 1F) * pow(2F, e - 15).toFloat * (1F + m / 1024F)
    } else if ((raw & 1023) != 0) {
      Float.NaN
    } else if (raw < 0) {
      Float.PositiveInfinity
    } else {
      Float.NegativeInfinity
    }
  }

  /**
    * String representation of this Float16 value.
    */
  override def toString: String =
    toFloat.toString
}

object Float16 {

  // interesting Float16 constants
  // with the exception of NaN, values go from smallest to largest
  val NaN               = new Float16(0x7c01.toShort)
  val NegativeInfinity  = new Float16(0x7c00.toShort)
  val MinValue          = new Float16(0x7bff.toShort)
  val MinusOne          = new Float16(0x3c00.toShort)
  val MaxNegativeNormal = new Float16(0x0400.toShort)
  val MaxNegative       = new Float16(0x0001.toShort)
  val NegativeZero      = new Float16(0x0000.toShort)
  val Zero              = new Float16(0x8000.toShort)
  val MinPositive       = new Float16(0x8001.toShort)
  val MinPositiveNormal = new Float16(0x8400.toShort)
  val One               = new Float16(0xbc00.toShort)
  val MaxValue          = new Float16(0xfbff.toShort)
  val PositiveInfinity  = new Float16(0xfc00.toShort)

  /**
    * Create a Float16 value from a Float.
    *
    * This value is guaranteed to be the closest possible Float16
    * value. However, because there are many more possible Float
    * values, rounding will occur, and very large or very small values
    * will end up as infinities.
    *
    * Given any (x: Float16), Float16.fromFloat(x.toFloat) = x
    *
    * The reverse is not necessarily true, since there are many Float
    * values which are not precisely representable as Float16 values.
    */
  def fromFloat(n: Float): Float16 = {

    // given e, an exponent in [-14, 15], and x, a double value,
    // return the Float16 value that is closest to (x * 2^e).
    def createFinite(e: Int, x: Float): Float16 = {
      def createNormal(e: Int, m: Int): Float16 =
        new Float16((((e + 15) << 10) | m | 0x8000).toShort)
      val m = round((x - 1F) * 1024).toInt
      if (e == -14 && m == 0) {
        Float16.MinPositiveNormal
      } else if (e == -14) {
        if (m == 0) Float16.Zero
        else if (x >= 1F) createNormal(e, m)
        else new Float16((0x8000 | round(x * 1024)).toShort)
      } else if (e == 15) {
        if (m < 1024) createNormal(e, m)
        else Float16.PositiveInfinity
      } else {
        createNormal(e, m)
      }
    }

    if (JFloat.isNaN(n)) Float16.NaN
    else if (n == Float.PositiveInfinity) Float16.PositiveInfinity
    else if (n == Float.NegativeInfinity) Float16.NegativeInfinity
    else if (JFloat.compare(n, -0F) == 0) Float16.NegativeZero
    else if (n == 0F) Float16.Zero
    else if (n < 0F) -fromFloat(-n)
    else if (1F <= n && n < 2F) createFinite(0, n)
    else {
      var e = 0
      var x = n
      if (n < 1F) {
        while (x < 1F && e > -14) { x *= 2F; e -= 1 }
      } else {
        while (x >= 2F && e < 15) { x *= 0.5F; e += 1 }
      }
      createFinite(e, x)
    }
  }
}

object Test {

  /**
    * Basic test of Float16.
    *
    * Since there are only 65k possible values, we can just iterate
    * through all of them to get "complete" test coverage.
    */
  def main(args: Array[String]): Unit = {
    var ok = Set.empty[Int]
    var fail = Set.empty[Int]
    var error = Set.empty[Int]

    var start = Short.MinValue.toInt
    val limit = Short.MaxValue.toInt + 1
    val total = limit - start
    while (start < limit) {
      try {
        val n0 = new Float16(start.toShort)
        val x0 = n0.toFloat
        val n1 = Float16.fromFloat(x0)
        val x1 = n1.toFloat

        val nr = new Float16(nextInt.toShort)
        val xr = nr.toFloat

        if (JFloat.compare(x0, x1) != 0) {
          fail += start
        } else if (JFloat.compare(n0.signum, signum(x0)) != 0) {
          fail += start
        } else if (n0.isNaN != JFloat.isNaN(x0)) {
          fail += start
        } else if (n0.isInfinite != JFloat.isInfinite(x0)) {
          fail += start
        } else if (n0.isFinite != JFloat.isFinite(x0)) {
          fail += start
        } else if (JFloat.compare((-n0).toFloat, -x0) != 0) {
          fail += start
        } else if (!n0.isNaN && !(n0 == n0)) {
          fail += start
        } else if (!n0.isNaN && !(n0 <= n0)) {
          fail += start
        } else if ((n0 < nr) != (x0 < xr)) {
          fail += start
        } else if ((n0 <= nr) != (x0 <= xr)) {
          fail += start
        } else if ((n0 == nr) != (x0 == xr)) {
          fail += start
        } else {
          ok += start
        }
      } catch { case e: Exception =>
        //throw e
        error += start
      } finally {
        start += 1
      }
    }
    println("ok:    %5d/%5d" format (ok.size, total))
    println("fail:  %5d/%5d" format (fail.size, total))
    println("error: %5d/%5d" format (error.size, total))
  }
}
