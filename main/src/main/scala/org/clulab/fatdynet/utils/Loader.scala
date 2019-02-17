package org.clulab.fatdynet.utils

import java.util.regex.Pattern

import edu.cmu.dynet.{
  Dim,
  LookupParameter,
  ModelLoader,
  ModelSaver,
  Parameter,
  ParameterCollection,

  FastLstmBuilder,
  LstmBuilder,
  CompactVanillaLSTMBuilder,
  CoupledLstmBuilder,
  VanillaLstmBuilder,

  // TreeLSTMBuilder, // abstract
    UnidirectionalTreeLSTMBuilder,
    BidirectionalTreeLSTMBuilder,

  RnnBuilder, // abstract
  SimpleRnnBuilder,

  GruBuilder
}
import org.clulab.fatdynet.utils.Closer.AutoCloser

import scala.collection.mutable
import scala.io.Source

/**
  * See https://dynet.readthedocs.io/en/latest/python_saving_tutorial.html
  */
abstract class Loader(path: String, namespace: String) {

  def namespaceFilter(objectName: String): Boolean = objectName.startsWith(namespace)

  def parameterFilter(objectType: String, objectName: String): Boolean =
      (objectType == "#Parameter#" || objectType == "#LookupParameter#") && !objectName.matches(".*/_[0-9]+$")

  def modelFilter(objectType: String, objectName: String): Boolean
  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder]

  protected def readParameters(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int],
      parameters: mutable.Map[String, Parameter], lookupParameters: mutable.Map[String, LookupParameter]): Boolean = {
    objectType match {
      case "#Parameter#" =>
        val pc = new ParameterCollection()
        val param = pc.addParameters(Dim(dims))
        modelLoader.populateParameter(param, key = objectName)
        parameters(objectName) = param
        true
      case "#LookupParameter#" =>
        val pc = new ParameterCollection()
        val param = pc.addLookupParameters(dims.last, Dim(dims.dropRight(1)))
        modelLoader.populateLookupParameter(param, key = objectName)
        lookupParameters(objectName) = param
        true
      case _ => false
    }
  }

  protected def readModel(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean

  protected def readLine(lineno: Int, line: String, modelLoader: ModelLoader, parameters: mutable.Map[String, Parameter],
      lookupParameters: mutable.Map[String, LookupParameter], useParameters: Boolean, useBuilder: Boolean): Unit = {
    val Array(objectType, objectName, dimension, _, _) = line.split(" ")
    // Skip leading { and trailing }
    val dims = dimension.substring(1, dimension.length - 1).split(",").map(_.toInt)

    if (namespaceFilter(objectName)) {
      if (modelFilter(objectType, objectName)) {
        if (useBuilder)
          if (!readModel(modelLoader, objectType, objectName, dims))
            throw new RuntimeException(s"Found unrecognized object type '$objectType' while reading model in line $lineno: '$line'")
      }
      else if (parameterFilter(objectType, objectName)) {
        if (useParameters)
          if (!readParameters(modelLoader, objectType, objectName, dims, parameters, lookupParameters))
            throw new RuntimeException(s"Found unrecognized object type '$objectType' while reading parameter in line $lineno: '$line'")
      }
      else
        throw new RuntimeException(s"Encountered unrecognized object type '$objectType' while reading line $lineno: '$line'")
    }
  }

  protected def load(useParameters: Boolean, useBuilder: Boolean):
      (Map[String, Parameter], Map[String, LookupParameter], Option[RnnBuilder], ParameterCollection) = {
    new Loader.ClosableModelLoader(path).autoClose { modelLoader =>
      Source.fromFile(path).autoClose { source =>
        val parameters = mutable.Map[String, Parameter]()
        val lookupParameters = mutable.Map[String, LookupParameter]()

        source
            .getLines
            //.zipWithIndex
            .filter(line => line.startsWith("#"))
            .foreach(line => readLine(5, line, modelLoader, parameters, lookupParameters, useParameters, useBuilder))

        val model = new ParameterCollection
        val optionBuilder = if (useBuilder) newBuilder(modelLoader, model) else None

        (parameters.toMap, lookupParameters.toMap, optionBuilder, model)
      }
    }
  }

  def loadParameters: (Map[String, Parameter], Map[String, LookupParameter]) = {
    val (parameters, lookupParameters, _, _) = load(useParameters = true, useBuilder = false)
    (parameters, lookupParameters)
  }

  def loadBuilder: (Option[RnnBuilder], ParameterCollection) = {
    val (_, _, builder, model) = load(useParameters = false, useBuilder = true)
    (builder, model)
  }

  def loadParametersAndBuilder: (Map[String, Parameter], Map[String, LookupParameter], Option[RnnBuilder], ParameterCollection) = {
    val (parameters, lookupParameters, builder, model) = load(useParameters = true, useBuilder = true)
    (parameters, lookupParameters, builder, model)
  }
}

class ParameterLoader(path: String, namespace: String) extends Loader(path, namespace) {

  def modelFilter(objectType: String, objectName: String): Boolean = !parameterFilter(objectType, objectName)

  protected def readModel(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean = true

  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] = None
}

abstract class SimpleLoader(path: String, namespace: String) extends Loader(path, namespace) {
  protected val pattern: Pattern
  protected var count: Int = 0
  protected var inputDim: Int = -1
  protected var hiddenDim: Int = -1
  protected var name: String = ""

  def modelFilter(objectType: String, objectName: String): Boolean =
      objectType == "#Parameter#" && pattern.matcher(objectName).matches

  protected def readModelSingleLine(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean = {
    val matcher = pattern.matcher(objectName)

    if (matcher.matches) {
      if (count == 0) {
        inputDim = dims.last
        hiddenDim = dims.head
        name = matcher.group(1)
      }
      else
        require(matcher.group(1) == name)
      count += 1
      true
    }
    else
      false
  }

  protected def readModelDoubleLine(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean = {
    val matcher = pattern.matcher(objectName)

    if (matcher.matches) {
      if (count == 0) {
        inputDim = dims.last
        name = matcher.group(1)
      }
      else {
        require(matcher.group(1) == name)
        if (count == 1)
          hiddenDim = dims.last
      }
      count += 1
      true
    }
    else
      false
  }

  protected def populate(builder: RnnBuilder, modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] = {
    modelLoader.populateModel(model, name)
    Some(builder)
  }
}

abstract class ComplexLoader(path: String, namespace: String) extends SimpleLoader(path, namespace) {
  protected val lnLstmPattern: Pattern = Loader.lnLstmPattern
  protected var lnLstmCount = 0

  override def modelFilter(objectType: String, objectName: String): Boolean =
    objectType == "#Parameter#" && (pattern.matcher(objectName).matches || lnLstmPattern.matcher(objectName).matches)

  protected def readModel(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean = {
    val matcher = pattern.matcher(objectName)
    val lnLstmMatcher = lnLstmPattern.matcher(objectName)

    if (matcher.matches) {
      if (count == 0) {
        inputDim = dims.last
        name = matcher.group(1)
      }
      else {
        require(matcher.group(1) == name)
        if (count == 1)
          hiddenDim = dims.last
      }
      count += 1
      true
    }
    else if (count > 0 && lnLstmMatcher.matches && lnLstmMatcher.group(1) == name) {
      lnLstmCount += 1
      true
    }
    else
      false
  }
}

class FastLstmLoader(path: String, namespace: String) extends SimpleLoader(path, namespace) {
  protected val pattern: Pattern = Loader.fastLstmLoaderPattern

  protected def readModel(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean =
      readModelSingleLine(modelLoader, objectType, objectName, dims)

  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] =
      if (count > 0 && count % 11 == 0)
        populate(new FastLstmBuilder(count / 11, inputDim, hiddenDim, model), modelLoader, model)
      else
        None
}

class LstmLoader(path: String, namespace: String) extends ComplexLoader(path, namespace) {
  protected val pattern: Pattern = Loader.lstmLoaderPattern

  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] =
      if (count > 0 && count % 3 == 0 && (lnLstmCount == 0 || lnLstmCount == count * 2))
        populate(new LstmBuilder(count / 3, inputDim, hiddenDim, model, lnLstmCount > 0), modelLoader, model)
      else
        None
}

class CompactVanillaLstmLoader(path: String, namespace: String) extends SimpleLoader(path, namespace) {
  protected val pattern: Pattern = Loader.compactVanillaLstmPattern

  protected def readModel(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean =
      readModelDoubleLine(modelLoader, objectType, objectName, dims)

  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] =
      if (count > 0 && count % 3 == 0)
        populate(new CompactVanillaLSTMBuilder(count / 3, inputDim, hiddenDim, model), modelLoader, model)
      else
        None
}

class CoupledLstmLoader(path: String, namespace: String) extends SimpleLoader(path, namespace) {
  protected val pattern: Pattern = Loader.coupledLstmPattern

  protected def readModel(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean =
      readModelSingleLine(modelLoader, objectType, objectName, dims)

  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] =
      if (count > 0 && count % 11 == 0)
        populate(new CoupledLstmBuilder(count / 11, inputDim, hiddenDim, model), modelLoader, model)
      else
        None
}

class VanillaLstmLoader(path: String, namespace: String) extends ComplexLoader(path, namespace) {
  protected val pattern: Pattern = Loader.vanillaLstmPattern

  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] =
      if (count > 0 && count % 3 == 0 && (lnLstmCount == 0 || lnLstmCount == count * 2))
        populate(new VanillaLstmBuilder(count / 3, inputDim, hiddenDim, model, lnLstmCount > 0), modelLoader, model)
      else
        None
}

class UnidirectionalTreeLstmLoader(path: String, namespace: String) extends SimpleLoader(path, namespace) {
  protected val pattern: Pattern = Loader.unidirectionalTreeLstmPattern

  protected def readModel(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean =
    readModelDoubleLine(modelLoader, objectType, objectName, dims)

  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] =
      if (count > 0 && count % 3 == 0)
        populate(new UnidirectionalTreeLSTMBuilder(count / 3, inputDim, hiddenDim, model), modelLoader, model)
      else
        None
}

class BidirectionalTreeLstmLoader(path: String, namespace: String) extends SimpleLoader(path, namespace) {
  protected val pattern: Pattern = Loader.bidirectionalTreeLstmPattern

  protected def readModel(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean =
      readModelSingleLine(modelLoader, objectType, objectName, dims)

  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] =
      if (count > 0 && count % 6 == 0)
        populate(new BidirectionalTreeLSTMBuilder(count / 6, inputDim, hiddenDim / 2, model), modelLoader, model)
      else
        None
}

class SimpleRnnLoader(path: String, namespace: String) extends SimpleLoader(path, namespace) {
  protected val pattern: Pattern = Loader.simpleRnnPattern
  protected var singleDimCount = 0

  protected def readModel(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean = {
    val matcher = pattern.matcher(objectName)

    if (matcher.matches) {
      if (count == 0) {
        inputDim = dims.last
        hiddenDim = dims.head
        name = matcher.group(1)
      }
      else {
        require(matcher.group(1) == name)
        if (dims.length == 1)
          singleDimCount += 1
      }
      count += 1
      true
    }
    else
      false
  }

  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] = {
    val ratio = count / singleDimCount
    val supportLags = ratio == 4

    if (count > 0 && count % singleDimCount == 0 && (ratio == 3 || ratio == 4))
      populate(new SimpleRnnBuilder(count / ratio, inputDim, hiddenDim, model, supportLags), modelLoader, model)
    else
      None
  }
}

class GruLoader(path: String, namespace: String) extends SimpleLoader(path, namespace) {
  protected val pattern: Pattern = Loader.gruPattern

  protected def readModel(modelLoader: ModelLoader, objectType: String, objectName: String, dims: Array[Int]): Boolean =
      readModelSingleLine(modelLoader, objectType, objectName, dims)

  def newBuilder(modelLoader: ModelLoader, model: ParameterCollection): Option[RnnBuilder] =
      if (count > 0 && count % 9 == 0)
        populate(new GruBuilder(count / 9, inputDim, hiddenDim, model), modelLoader, model)
      else
        None
}

object Loader {
  lazy val fastLstmLoaderPattern: Pattern = "(.*)/fast-lstm-builder/_[0-9]+$".r.pattern
  lazy val lstmLoaderPattern: Pattern = "(.*)/vanilla-lstm-builder/_[0-9]+$".r.pattern
  lazy val compactVanillaLstmPattern: Pattern = "(.*)/compact-vanilla-lstm-builder/_[0-9]+$".r.pattern
  lazy val coupledLstmPattern: Pattern = "(.*)/lstm-builder/_[0-9]+$".r.pattern
  lazy val vanillaLstmPattern: Pattern = "(.*)/vanilla-lstm-builder/_[0-9]+$".r.pattern
  lazy val unidirectionalTreeLstmPattern: Pattern = "(.*)/unidirectional-tree-lstm-builder/vanilla-lstm-builder/_[0-9]+$".r.pattern
  lazy val bidirectionalTreeLstmPattern: Pattern = "(.*)/bidirectional-tree-lstm-builder/vanilla-lstm-builder(_1)?/_[0-9]+$".r.pattern
  lazy val simpleRnnPattern: Pattern = "(.*)/simple-rnn-builder/_[0-9]+$".r.pattern
  lazy val gruPattern: Pattern = "(.*)/gru-builder/_[0-9]+$".r.pattern

  lazy val lnLstmPattern: Pattern = "(.*)/_[0-9]+$".r.pattern

  class ClosableModelLoader(filename: String) extends ModelLoader(filename) {
    def close(): Unit = done
  }

  class ClosableModelSaver(filename: String) extends ModelSaver(filename) {
    def close(): Unit = done
  }

  def loadParameters(path: String, namespace: String = ""): (Map[String, Parameter], Map[String, LookupParameter]) = {
    new ParameterLoader(path, namespace).loadParameters
  }

  def loadFastLstm(path: String, namespace: String = ""): (Option[FastLstmBuilder], Option[ParameterCollection], Map[String, Parameter], Map[String, LookupParameter]) = {
    val (parameters, lookupParameters, someBuilder: Option[RnnBuilder], model) = new FastLstmLoader(path, namespace).loadParametersAndBuilder
    (someBuilder.map(_.asInstanceOf[FastLstmBuilder]), Some(model), parameters, lookupParameters)
  }

  def loadLstm(path: String, namespace: String = ""): (Option[LstmBuilder], Option[ParameterCollection], Map[String, Parameter], Map[String, LookupParameter]) = {
    val (parameters, lookupParameters, someBuilder: Option[RnnBuilder], model) = new LstmLoader(path, namespace).loadParametersAndBuilder
    (someBuilder.map(_.asInstanceOf[LstmBuilder]), Some(model), parameters, lookupParameters)
  }

  def loadCompactVanillaLstm(path: String, namespace: String = ""): (Option[CompactVanillaLSTMBuilder], Option[ParameterCollection], Map[String, Parameter], Map[String, LookupParameter]) = {
    val (parameters, lookupParameters, someBuilder: Option[RnnBuilder], model) = new CompactVanillaLstmLoader(path, namespace).loadParametersAndBuilder
    (someBuilder.map(_.asInstanceOf[CompactVanillaLSTMBuilder]), Some(model), parameters, lookupParameters)
  }

  def loadCoupledLstm(path: String, namespace: String = ""): (Option[CoupledLstmBuilder], Option[ParameterCollection], Map[String, Parameter], Map[String, LookupParameter]) = {
    val (parameters, lookupParameters, someBuilder: Option[RnnBuilder], model) = new CoupledLstmLoader(path, namespace).loadParametersAndBuilder
    (someBuilder.map(_.asInstanceOf[CoupledLstmBuilder]), Some(model), parameters, lookupParameters)
  }

  def loadVanillaLstm(path: String, namespace: String = ""): (Option[VanillaLstmBuilder], Option[ParameterCollection], Map[String, Parameter], Map[String, LookupParameter]) = {
    val (parameters, lookupParameters, someBuilder: Option[RnnBuilder], model) = new VanillaLstmLoader(path, namespace).loadParametersAndBuilder
    (someBuilder.map(_.asInstanceOf[VanillaLstmBuilder]), Some(model), parameters, lookupParameters)
  }

  def loadUnidirectionalTreeLstm(path: String, namespace: String = ""): (Option[UnidirectionalTreeLSTMBuilder], Option[ParameterCollection], Map[String, Parameter], Map[String, LookupParameter]) = {
    val (parameters, lookupParameters, someBuilder: Option[RnnBuilder], model) = new UnidirectionalTreeLstmLoader(path, namespace).loadParametersAndBuilder
    (someBuilder.map(_.asInstanceOf[UnidirectionalTreeLSTMBuilder]), Some(model), parameters, lookupParameters)
  }

  def loadBidirectionalTreeLstm(path: String, namespace: String = ""): (Option[BidirectionalTreeLSTMBuilder], Option[ParameterCollection], Map[String, Parameter], Map[String, LookupParameter]) = {
    val (parameters, lookupParameters, someBuilder: Option[RnnBuilder], model) = new BidirectionalTreeLstmLoader(path, namespace).loadParametersAndBuilder
    (someBuilder.map(_.asInstanceOf[BidirectionalTreeLSTMBuilder]), Some(model), parameters, lookupParameters)
  }

  def loadSimpleRnn(path: String, namespace: String = ""): (Option[SimpleRnnBuilder], Option[ParameterCollection], Map[String, Parameter], Map[String, LookupParameter]) = {
    val (parameters, lookupParameters, someBuilder: Option[RnnBuilder], model) = new SimpleRnnLoader(path, namespace).loadParametersAndBuilder
    (someBuilder.map(_.asInstanceOf[SimpleRnnBuilder]), Some(model), parameters, lookupParameters)
  }

  def loadGru(path: String, namespace: String = ""): (Option[GruBuilder], Option[ParameterCollection], Map[String, Parameter], Map[String, LookupParameter]) = {
    val (parameters, lookupParameters, someBuilder: Option[RnnBuilder], model) = new GruLoader(path, namespace).loadParametersAndBuilder
    (someBuilder.map(_.asInstanceOf[GruBuilder]), Some(model), parameters, lookupParameters)
  }
}
