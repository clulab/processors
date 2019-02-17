package org.clulab.fatdynet.design

import edu.cmu.dynet.ParameterCollection
import edu.cmu.dynet._

import scala.collection.mutable.ListBuffer

class Artifact(val name: String, val parameter: Option[Parameter],
    val lookupParameter: Option[LookupParameter], val rnnBuilder: Option[RnnBuilder]) {

  def this(name: String, parameter: Parameter) = this(name, Some(parameter), None, None)

  def this(name: String, lookupParameter: LookupParameter) = this(name, None, Some(lookupParameter), None)

  def this(name: String, rnnBuilder: RnnBuilder) = this(name, None, None, Some(rnnBuilder))

  def isParameter: Boolean = parameter.isDefined

  def isLookupParameter: Boolean = lookupParameter.isDefined

  def isRnnBuilder: Boolean = rnnBuilder.isDefined

  def populate(modelLoader: ModelLoader, parameterCollection: ParameterCollection): Unit = {
    if (isParameter)
      modelLoader.populateParameter(parameter.get, name)
    else if (isLookupParameter)
      modelLoader.populateLookupParameter(lookupParameter.get, name)
    else if (isRnnBuilder)
      modelLoader.populateModel(parameterCollection, name)
  }
}

abstract class Design(val name: String, val index: Option[Int]) {

  def build(parameterCollection: ParameterCollection): Artifact

  def isPotentiallyReorderable = false

  def isActuallyReorderable = isPotentiallyReorderable && index.nonEmpty
}

class ParameterDesign(name: String, index: Option[Int], val dims: Dim)
    extends Design(name, index) {

  override def isPotentiallyReorderable: Boolean = true

  override def build(parameterCollection: ParameterCollection): Artifact =
      new Artifact(name, parameterCollection.addParameters(dims))
}

class LookupParameterDesign(name: String, index: Option[Int], val n: Long, val dims: Dim)
    extends Design(name, index) {

  override def isPotentiallyReorderable: Boolean = true

  override def build(parameterCollection: ParameterCollection): Artifact =
      new Artifact(name, parameterCollection.addLookupParameters(n, dims))
}

abstract class RnnBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
    val layers: Long, val inputDim: Long, val hiddenDim: Long)
    extends Design(name, None) {

  def newArtifact(rnnBuilder: RnnBuilder): Artifact = new Artifact(name, rnnBuilder)
}

class CompactVanillaLstmBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
    layers: Long, inputDim: Long, hiddenDim: Long)
    extends RnnBuilderDesign(name, globalIndex, localIndex, layers, inputDim, hiddenDim) {

  override def build(parameterCollection: ParameterCollection): Artifact =
      newArtifact(new CompactVanillaLSTMBuilder(layers, inputDim, hiddenDim, parameterCollection))
}

class CoupledLstmBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
    layers: Long, inputDim: Long, hiddenDim: Long)
    extends RnnBuilderDesign(name, globalIndex, localIndex, layers, inputDim, hiddenDim) {

  override def build(parameterCollection: ParameterCollection): Artifact =
      newArtifact(new CoupledLstmBuilder(layers, inputDim, hiddenDim, parameterCollection))
}

class FastLstmBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
    layers: Long, inputDim: Long, hiddenDim: Long)
    extends RnnBuilderDesign(name, globalIndex, localIndex, layers, inputDim, hiddenDim) {

  override def build(parameterCollection: ParameterCollection): Artifact =
      newArtifact(new FastLstmBuilder(layers, inputDim, hiddenDim, parameterCollection))
}

class GruBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
    layers: Long, inputDim: Long, hiddenDim: Long)
    extends RnnBuilderDesign(name, globalIndex, localIndex, layers, inputDim, hiddenDim) {

  override def build(parameterCollection: ParameterCollection): Artifact =
      newArtifact(new GruBuilder(layers, inputDim, hiddenDim, parameterCollection))
}

class LstmBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
  layers: Long, inputDim: Long, hiddenDim: Long, val lnLSTM: Boolean)
    extends RnnBuilderDesign(name, globalIndex, localIndex, layers, inputDim, hiddenDim) {

  override def build(parameterCollection: ParameterCollection): Artifact  =
      newArtifact(new LstmBuilder(layers, inputDim, hiddenDim, parameterCollection, lnLSTM))
}

abstract class TreeLstmBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
    layers: Long, inputDim: Long, hiddenDim: Long)
    extends RnnBuilderDesign(name, globalIndex, localIndex, layers, inputDim, hiddenDim)

class BidirectionalTreeLstmBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
    layers: Long, inputDim: Long, hiddenDim: Long)
    extends TreeLstmBuilderDesign(name, globalIndex, localIndex, layers, inputDim, hiddenDim) {

  override def build(parameterCollection: ParameterCollection): Artifact =
      newArtifact(new BidirectionalTreeLSTMBuilder(layers, inputDim, hiddenDim, parameterCollection))
}

class UnidirectionalTreeLstmBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
    layers: Long, inputDim: Long, hiddenDim: Long)
    extends TreeLstmBuilderDesign(name, globalIndex, localIndex, layers, inputDim, hiddenDim) {

  override def build(parameterCollection: ParameterCollection): Artifact =
      newArtifact(new UnidirectionalTreeLSTMBuilder(layers, inputDim, hiddenDim, parameterCollection))
}

class SimpleRnnBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
    layers: Long, inputDim: Long, hiddenDim: Long, val supportLags: Boolean)
    extends RnnBuilderDesign(name, globalIndex, localIndex, layers, inputDim, hiddenDim) {

  override def build(parameterCollection: ParameterCollection): Artifact =
      newArtifact(new SimpleRnnBuilder(layers, inputDim, hiddenDim, parameterCollection))
}

class VanillaLstmBuilderDesign(name: String, globalIndex: Int, localIndex: Int,
    layers: Long, inputDim: Long, hiddenDim: Long, val lnLSTM: Boolean)
    extends RnnBuilderDesign(name, globalIndex, localIndex, layers, inputDim, hiddenDim) {

  override def build(parameterCollection: ParameterCollection): Artifact =
      newArtifact(new VanillaLstmBuilder(layers, inputDim, hiddenDim, parameterCollection, lnLSTM))
}
