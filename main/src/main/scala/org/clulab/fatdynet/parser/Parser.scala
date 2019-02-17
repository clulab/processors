package org.clulab.fatdynet.parser

import java.util.regex.Matcher
import java.util.regex.Pattern

import edu.cmu.dynet._
import org.clulab.fatdynet.design._
import org.clulab.fatdynet.utils.Header

abstract class Parser(val name: String) {

  def parse(header: Header): Boolean = false

  def finish(): Design
}

abstract class ParamParser(name: String, val dims: Array[Int], index: Option[Int]) extends Parser(name)

object ParamParser {
  val nameRegex = "^(.*)"
  val indexRegex = "_(0|[1-9][0-9]*)$"
  val pattern = (nameRegex + "/" + indexRegex).r.pattern

  def getNameAndIndex(objectName: String): (String, Option[Int]) = {
    val matcher = pattern.matcher(objectName)

    if (matcher.matches) {
      val name = matcher.group(1)
      val index = Option(matcher.group(2)).map(_.toInt)

      (name, index)
    }
    else (objectName, None)
  }
}

class ParameterParser(name: String, dims: Array[Int], index: Option[Int]) extends ParamParser(name, dims, index) {

  override def parse(header: Header): Boolean = false // Only single line possible

  override def finish(): Design = {
    new ParameterDesign(name, index, Dim(dims))
  }
}

object ParameterParser {
  val objectType = "#Parameter#"

  def mkParser(header: Header): Option[Parser] = {
    if (header.objectType != objectType) None
    else {
      val (name, index) = ParamParser.getNameAndIndex(header.objectName)

      Some(new ParameterParser(name, header.dims, index))
    }
  }
}

class LookupParameterParser(name: String, dims: Array[Int], index: Option[Int]) extends ParamParser(name, dims, index) {

  override def parse(header: Header): Boolean = false // Only single line possible

  override def finish(): Design = {
    new LookupParameterDesign(name, index, dims.last, Dim(dims.dropRight(1)))
  }
}

object LookupParameterParser {
  val objectType = "#LookupParameter#"

  def mkParser(header: Header): Option[Parser] = {
    if (header.objectType != objectType) None
    else {
      val (name, index) = ParamParser.getNameAndIndex(header.objectName)

      Some(new LookupParameterParser(name, header.dims, index))
    }
  }
}

abstract class RnnParser(name: String, val outerIndex: Int) extends Parser(name) {
  protected val pattern: Pattern
  protected var innerIndex: Int = 0 // Already processed first line
  protected var inputDim: Int = -1
  protected var hiddenDim: Int = -1
  protected var count: Int = 0 // Number of lines overall

  def isMatch(matcher: Matcher): Boolean = {
    matcher.matches &&
        RnnParser.getName(matcher) == name &&
        RnnParser.getOuterIndex(matcher) == outerIndex &&
        RnnParser.getInnerIndex(matcher) == innerIndex
  }

  def innerIndexDivideOrThrow(divisor: Int): Int = {
    if (!(innerIndex > 0 && innerIndex % divisor == 0))
      throw new Exception(s"For the ${this.getClass.getSimpleName}, the innerIndex should be a positive multiple of $divisor, but it was $innerIndex.")
    innerIndex / divisor
  }
}

object RnnParser {
  val objectType = "#Parameter#"
  val nameRegex = "^(.*)"
  val outerIndexRegex = "(_[1-9][0-9]*)?"
  val innerIndexRegex = "_(0|[1-9][0-9]*)$"

  def pattern(builderType: String): Pattern = (nameRegex + "/" + builderType + outerIndexRegex + "/" + innerIndexRegex).r.pattern

  def getName(matcher: Matcher): String = matcher.group(1)

  def getOuterIndex(matcher: Matcher): Int = {
    Option(matcher.group(2)).map(_.substring(1).toInt).getOrElse(0)
  }

  def getInnerIndex(matcher: Matcher): Int = {
    matcher.group(matcher.groupCount).toInt
  }

  def mkParser(header: Header, pattern: Pattern, maker: (String, Int) => RnnParser): Option[Parser] = {
    val matcher = pattern.matcher(header.objectName)

    if (header.objectType != objectType || !matcher.matches() || getInnerIndex(matcher) != 0) None
    else Some(maker(getName(matcher), getOuterIndex(matcher)))
  }
}

abstract class SimpleParser(name: String, outerIndex: Int) extends RnnParser(name, outerIndex)

/**
  * This parser has to read only one line in order to figure out the number of hidden layers.
  */
abstract class SimpleSingleParser(name: String, outerIndex: Int) extends SimpleParser(name, outerIndex) {

  override def parse(header: Header): Boolean = {
    val matcher = pattern.matcher(header.objectName)

    if (isMatch(matcher)) {
      if (innerIndex == 0) {
        inputDim = header.dims.last
        hiddenDim = header.dims.head
      }
      innerIndex += 1
      count += 1
      true
    }
    else false
  }
}

/**
  * This parser has to read two lines in order to figure out the number of hidden layers.
  */
abstract class SimpleDoubleParser(name: String, outerIndex: Int) extends SimpleParser(name, outerIndex) {

  override def parse(header: Header): Boolean = {
    val matcher = pattern.matcher(header.objectName)

    if (isMatch(matcher)) {
      if (innerIndex == 0)
          inputDim = header.dims.last
      else if (innerIndex == 1)
          hiddenDim = header.dims.last
      innerIndex += 1
      count += 1
      true
    }
    else false
  }
}

/**
  * This parser has to keep track of the lnLstmCount which means it always reads two lines.
  */
abstract class ComplexParser(name: String, outerIndex: Int) extends RnnParser(name, outerIndex) {
  protected val lnLstmPattern: Pattern = ComplexParser.lnLstmPattern
  protected var lnLstmCount = 0
  protected var lnLstmSize = -1

  override def parse(header: Header): Boolean = {
    val matcher = pattern.matcher(header.objectName)

    if (isMatch(matcher)) {
      if (innerIndex == 0) {
        inputDim = header.dims.last
        lnLstmSize = header.dims.head
      }
      else if (innerIndex == 1)
        hiddenDim = header.dims.last
      innerIndex += 1
      count += 1
      true
    }
    else {
      val lnLstmMatcher = lnLstmPattern.matcher(header.objectName)
      // We can't tell a normal variable apart from the lnLstm values, but the dimension is a good hint.
      if (count > 0 && header.dims.head == lnLstmSize && lnLstmMatcher.matches && ComplexParser.getLstmName(lnLstmMatcher) == name) {
          // && ComplexParser.getLstmIndex(lnLstmMatcher) == lnLstmCount) {
          // This will not work if there are multiple ComplexRnns in the same file.  The index is not reset.
        lnLstmCount += 1
        count += 1
        true
      }
      else false
    }
  }
}

object ComplexParser {
  val lnLstmPattern: Pattern = (RnnParser.nameRegex + "/" + RnnParser.innerIndexRegex).r.pattern

  def getLstmIndex(matcher: Matcher): Int = {
    matcher.group(matcher.groupCount).toInt
  }

  def getLstmName(matcher: Matcher): String = matcher.group(1)
}

class CompactVanillaLstmParser(name: String, outerIndex: Int) extends SimpleDoubleParser(name, outerIndex) {
  protected val pattern: Pattern = CompactVanillaLstmParser.pattern

  def finish(): Design = {
    new CompactVanillaLstmBuilderDesign(name, 3, 4, layers = innerIndexDivideOrThrow(3), inputDim, hiddenDim)
  }
}

object CompactVanillaLstmParser {
  val builderType = "compact-vanilla-lstm-builder"
  val pattern: Pattern = RnnParser.pattern(builderType)

  def mkParser(header: Header): Option[Parser] = {
    RnnParser.mkParser(header, pattern, (name: String, outerIndex: Int) =>
      new CompactVanillaLstmParser(name, outerIndex))
  }
}

class CoupledLstmParser(name: String, outerIndex: Int) extends SimpleSingleParser(name, outerIndex) {
  protected val pattern: Pattern = CoupledLstmParser.pattern

  def finish(): Design = {
    new CoupledLstmBuilderDesign(name, 3, 4, layers = innerIndexDivideOrThrow(11), inputDim, hiddenDim)
  }
}

object CoupledLstmParser {
  val builderType = "lstm-builder"
  val pattern: Pattern = RnnParser.pattern(builderType)

  def mkParser(header: Header): Option[Parser] = {
    RnnParser.mkParser(header, pattern, (name: String, outerIndex: Int) =>
        new CoupledLstmParser(name, outerIndex))
  }
}

class FastLstmParser(name: String, outerIndex: Int) extends SimpleSingleParser(name, outerIndex) {
  protected val pattern: Pattern = FastLstmParser.pattern

  def finish(): Design = {
    new FastLstmBuilderDesign(name, 3, 5, layers = innerIndexDivideOrThrow(11), inputDim, hiddenDim)
  }
}

object FastLstmParser {
  val builderType = "fast-lstm-builder"
  val pattern: Pattern = RnnParser.pattern(builderType)

  def mkParser(header: Header): Option[Parser] = {
    RnnParser.mkParser(header, pattern, (name: String, outerIndex: Int) =>
      new FastLstmParser(name, outerIndex))
  }
}

class GruParser(name: String, outerIndex: Int) extends SimpleSingleParser(name, outerIndex) {
  protected val pattern: Pattern = GruParser.pattern

  def finish(): Design = {
    new GruBuilderDesign(name, 4, 6, layers = innerIndexDivideOrThrow(9), inputDim, hiddenDim)
  }
}

object GruParser {
  val builderType = "gru-builder"
  val pattern: Pattern = RnnParser.pattern(builderType)

  def mkParser(header: Header): Option[Parser] = {
    RnnParser.mkParser(header, pattern, (name: String, outerIndex: Int) =>
        new GruParser(name, outerIndex))
  }
}

class LstmParser(name: String, outerIndex: Int) extends ComplexParser(name, outerIndex) {
  protected val pattern: Pattern = LstmParser.pattern

  def finish(): Design = {
    if (!(innerIndex > 0 && innerIndex % 3 == 0 && (lnLstmCount == 0 || lnLstmCount == innerIndex * 2)))
      throw new Exception(s"For the ${this.getClass.getSimpleName}, the innerIndex should be a positive multiple of 3.  It is $innerIndex.  " +
          s"Also, lnLstmCount should be 0 or twice innerIndex.  It is $lnLstmCount.")
    new LstmBuilderDesign(name, 5, 4, layers = innerIndex / 3, inputDim, hiddenDim, lnLstmCount > 0)
  }
}

object LstmParser {
  val builderType = "vanilla-lstm-builder"
  val pattern: Pattern = RnnParser.pattern(builderType)

  def mkParser(header: Header): Option[Parser] = {
    RnnParser.mkParser(header, pattern, (name: String, outerIndex: Int) =>
        new LstmParser(name, outerIndex))
  }
}

class BidirectionalTreeLstmParser(name: String, outerIndex: Int) extends SimpleParser(name, outerIndex) {
  protected val pattern: Pattern = BidirectionalTreeLstmParser.pattern
  protected var innerIndex2: Int = 0

  override def isMatch(matcher: Matcher): Boolean = {
    matcher.matches && {
      val newName = RnnParser.getName(matcher)
      val newOuterIndex = RnnParser.getOuterIndex(matcher)
      val newInnerIndex = RnnParser.getInnerIndex(matcher)
      val biIndex = BidirectionalTreeLstmParser.getBiIndex(matcher)

      newName == name && newOuterIndex == outerIndex && (
        (biIndex == 0 && newInnerIndex == innerIndex) ||
        (biIndex == 1 && newInnerIndex == innerIndex2)
      )
    }
  }

  override def parse(header: Header): Boolean = {
    val matcher = pattern.matcher(header.objectName)

    if (isMatch(matcher)) {
      val biIndex = BidirectionalTreeLstmParser.getBiIndex(matcher)

      if (biIndex == 0) {
        if (innerIndex == 0) {
          inputDim = header.dims.last
          hiddenDim = header.dims.head
        }
        innerIndex += 1
      }
      else
        innerIndex2 += 1
      count += 1
      true
    }
    else false
  }

  def finish(): Design = {
    if (innerIndex != innerIndex2)
      throw new Exception(s"For the ${this.getClass.getSimpleName}, the two inner indexes must be equal, but they are $innerIndex and $innerIndex2.")
    new BidirectionalTreeLstmBuilderDesign(name, 5, 4, layers = innerIndexDivideOrThrow(3), inputDim, hiddenDim / 2)
  }
}

object BidirectionalTreeLstmParser {
  val builderType = "bidirectional-tree-lstm-builder"
  val pattern: Pattern = (RnnParser.nameRegex + "/" +  builderType + RnnParser.outerIndexRegex + "/vanilla-lstm-builder(_1)?" + "/" + RnnParser.innerIndexRegex).r.pattern

  def getBiIndex(matcher: Matcher): Int = {
    Option(matcher.group(3)).map(_.substring(1).toInt).getOrElse(0)
  }

  def mkParser(header: Header): Option[Parser] = {
    RnnParser.mkParser(header, pattern, (name: String, outerIndex: Int) =>
        new BidirectionalTreeLstmParser(name, outerIndex))
  }
}

class UnidirectionalTreeLstmParser(name: String, outerIndex: Int) extends SimpleDoubleParser(name, outerIndex) {
  protected val pattern: Pattern = UnidirectionalTreeLstmParser.pattern

  def finish(): Design = {
    new UnidirectionalTreeLstmBuilderDesign(name, 5, 3, layers = innerIndexDivideOrThrow(3), inputDim, hiddenDim)
  }
}

object UnidirectionalTreeLstmParser {
  val builderType = "unidirectional-tree-lstm-builder"
  val pattern: Pattern = (RnnParser.nameRegex + "/" +  builderType + RnnParser.outerIndexRegex + "/vanilla-lstm-builder/" + RnnParser.innerIndexRegex).r.pattern

  def mkParser(header: Header): Option[Parser] = {
    RnnParser.mkParser(header, pattern, (name: String, outerIndex: Int) =>
        new UnidirectionalTreeLstmParser(name, outerIndex))
  }
}

class SimpleRnnParser(name: String, outerIndex: Int) extends RnnParser(name, outerIndex) {
  protected val pattern: Pattern = SimpleRnnParser.pattern
  protected var singleDimCount = 0

  override def parse(header: Header): Boolean = {
    val matcher = pattern.matcher(header.objectName)

    if (isMatch(matcher)) {
      if (innerIndex == 0) {
        inputDim = header.dims.last
        hiddenDim = header.dims.head
      }
      else
        if (header.dims.length == 1)
          singleDimCount += 1
      innerIndex += 1
      count += 1
      true
    }
    else false
  }

  def finish(): Design = {
    if (!(singleDimCount > 0))
      throw new Exception(s"For the ${this.getClass.getSimpleName}, singleDimCount must be non-zero, but it is $singleDimCount.")

    val ratio = innerIndex / singleDimCount
    val supportLags = ratio == 4

    if (!(innerIndex > 0 && innerIndex % singleDimCount == 0 && (ratio == 3 || ratio == 4)))
      throw new Exception(s"For the ${this.getClass.getSimpleName}, the innerIndex of $innerIndex should be a positive multiple of singleDimCount, which is $singleDimCount.  " +
          s"The multiple should be 3 or 4.  It is $ratio.")
    new SimpleRnnBuilderDesign(name, 5, 4, layers = innerIndex / ratio, inputDim, hiddenDim, supportLags)
  }
}

object SimpleRnnParser {
  val builderType = "simple-rnn-builder"
  val pattern: Pattern = RnnParser.pattern(builderType)

  def mkParser(header: Header): Option[Parser] = {
    RnnParser.mkParser(header, pattern, (name: String, outerIndex: Int) =>
        new SimpleRnnParser(name, outerIndex))
  }
}

class VanillaLstmParser(name: String, outerIndex: Int) extends ComplexParser(name, outerIndex) {
  protected val pattern: Pattern = VanillaLstmParser.pattern

  def finish(): Design = {
    val divisor = 3

    if (!(innerIndex > 0 && innerIndex % divisor == 0 && (lnLstmCount == 0 || lnLstmCount == innerIndex * 2)))
      throw new Exception(s"For the ${this.getClass.getSimpleName}, the innerIndex should be a positive multiple of $divisor.  It is is $innerIndex.  " +
          s"Also, lnLstmCount should be 0 or twice innerIndex.  It is $lnLstmCount.")
    new VanillaLstmBuilderDesign(name, 3, 5, layers = innerIndex / divisor, inputDim, hiddenDim, lnLstmCount > 0)
  }
}

object VanillaLstmParser {
  val builderType = "vanilla-lstm-builder"
  val pattern: Pattern = RnnParser.pattern(builderType)

  def mkParser(header: Header): Option[Parser] = {
    RnnParser.mkParser(header, pattern, (name: String, outerIndex: Int) =>
        new VanillaLstmParser(name, outerIndex))
  }
}
