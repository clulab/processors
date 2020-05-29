package org.clulab.dynet

import java.io.PrintWriter

import edu.cmu.dynet.{ComputationGraph, Expression, ExpressionVector, ParameterCollection}
import org.clulab.struct.Counter
import org.clulab.utils.Configured
import org.clulab.dynet.Utils._

import scala.collection.mutable.ArrayBuffer

/**
 * A sequence of layers that implements a complete NN architecture for sequence modeling
 */
class Layers (val initialLayer: Option[InitialLayer],
              val intermediateLayers: IndexedSeq[IntermediateLayer],
              val finalLayer: Option[FinalLayer]) extends Saveable {

  def outDim: Option[Int] = {
    if(finalLayer.nonEmpty) {
      return Some(finalLayer.get.outDim)
    }

    if(intermediateLayers.nonEmpty) {
      return Some(intermediateLayers.last.outDim)
    }

    if(initialLayer.nonEmpty) {
      return Some(initialLayer.get.outDim)
    }

    None
  }

  override def toString: String = {
    val sb = new StringBuilder
    var started = false
    if(initialLayer.nonEmpty) {
      sb.append("initial = " + initialLayer.get)
      started = true
    }
    for(i <- intermediateLayers.indices) {
      if(started) sb.append(" ")
      sb.append(s"intermediate (${i + 1}) = " + intermediateLayers(i))
      started = true
    }
    if(finalLayer.nonEmpty) {
      if(started) sb.append(" ")
      sb.append("final = " + finalLayer.get)
    }
    sb.toString()
  }

  def isEmpty: Boolean = initialLayer.isEmpty && intermediateLayers.isEmpty && finalLayer.isEmpty
  def nonEmpty: Boolean = ! isEmpty

  def loss(sentence: AnnotatedSentence,
           goldLabels: IndexedSeq[String],
           predicatePosition: Option[Int] = None): Expression = {
    assert(initialLayer.nonEmpty)
    assert(finalLayer.nonEmpty)

    var states = initialLayer.get.forward(sentence, predicatePosition, doDropout = true)
    for(i <- intermediateLayers.indices) {
      states = intermediateLayers(i).forward(states, doDropout = true)
    }
    states = finalLayer.get.forward(states, predicatePosition, doDropout = true)

    finalLayer.get.loss(states, goldLabels)
  }

  // strictly for testing purposes (doDropout = false for all layers)
  protected def forward(sentence: AnnotatedSentence,
                        predicatePosition: Option[Int] = None): ExpressionVector = {
    if(initialLayer.isEmpty) {
      throw new RuntimeException(s"ERROR: you can't call forward() on a Layers object that does not have an initial layer: $toString!")
    }

    var states = initialLayer.get.forward(sentence, predicatePosition, doDropout = false)

    for (i <- intermediateLayers.indices) {
      states = intermediateLayers(i).forward(states, doDropout = false)
    }

    if(finalLayer.nonEmpty) {
      states = finalLayer.get.forward(states, predicatePosition, doDropout = false)
    }

    states
  }

  protected def forwardFrom(inStates: ExpressionVector,
                            predicatePosition: Option[Int] = None): ExpressionVector = {
    if(initialLayer.nonEmpty) {
      throw new RuntimeException(s"ERROR: you can't call forwardFrom() on a Layers object that has an initial layer: $toString!")
    }

    var states = inStates

    for (i <- intermediateLayers.indices) {
      states = intermediateLayers(i).forward(states, doDropout = false)
    }

    if(finalLayer.nonEmpty) {
      states = finalLayer.get.forward(states, predicatePosition, doDropout = false)
    }

    states
  }

  def predict(sentence: AnnotatedSentence,
              predicatePosition: Option[Int] = None): IndexedSeq[String] = {
    assert(initialLayer.nonEmpty)
    assert(finalLayer.nonEmpty)

    val emissionScores: Array[Array[Float]] =
      this.synchronized { // DyNet's computation graph is a static variable, so this block must be synchronized

        ComputationGraph.renew()
        val states = forward(sentence, predicatePosition)
        Utils.emissionScoresToArrays(states)
      }

    val labels = finalLayer.get.inference(emissionScores)

    labels
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    if(initialLayer.nonEmpty) {
      save(printWriter, 1, "hasInitial")
      initialLayer.get.saveX2i(printWriter)
    } else {
      save(printWriter, 0, "hasInitial")
    }

    save(printWriter, intermediateLayers.length, "intermediateCount")
    for(il <- intermediateLayers) {
      il.saveX2i(printWriter)
    }

    if(finalLayer.nonEmpty) {
      save(printWriter, 1, "hasFinal")
      finalLayer.get.saveX2i(printWriter)
    } else {
      save(printWriter, 0, "hasFinal")
    }
  }
}

object Layers {
  def apply(config: Configured,
            paramPrefix: String,
            parameters: ParameterCollection,
            wordCounter: Counter[String],
            labelCounterOpt: Option[Counter[String]],
            hasPredicate: Boolean,
            providedInputSize: Option[Int]): Layers = {
    val initialLayer = EmbeddingLayer.initialize(config, paramPrefix + ".initial", parameters, wordCounter)

    var inputSize =
      if(initialLayer.nonEmpty) {
        Some(initialLayer.get.outDim)
      } else if(providedInputSize.nonEmpty) {
        providedInputSize
      } else {
        None
      }

    val intermediateLayers = new ArrayBuffer[IntermediateLayer]()
    var done = false
    for(i <- 1 to MAX_INTERMEDIATE_LAYERS if ! done) {
      if(inputSize.isEmpty) {
        throw new RuntimeException("ERROR: trying to construct an intermediate layer without a known input size!")
      }
      val intermediateLayer = RnnLayer.initialize(config, paramPrefix + s".intermediate$i", parameters, inputSize.get)
      if(intermediateLayer.nonEmpty) {
        intermediateLayers += intermediateLayer.get
        inputSize = Some(intermediateLayer.get.outDim)
      } else {
        done = true
      }
    }

    val finalLayer =
      if(labelCounterOpt.nonEmpty) {
        if(inputSize.isEmpty) {
          throw new RuntimeException("ERROR: trying to construct a final layer without a known input size!")
        }

        ForwardLayer.initialize(config, paramPrefix + ".final", parameters,
          labelCounterOpt.get, hasPredicate, inputSize.get)
      } else {
        None
      }

    new Layers(initialLayer, intermediateLayers, finalLayer)
  }

  /** Merges one shared Layers with a task-specific Layers object */
  def merge(sharedLayers: Layers, taskLayers: Layers): Layers = {
    // the task Layers must have a final layer
    if(taskLayers.finalLayer.isEmpty) {
      throw new RuntimeException(s"ERROR: did not find final layer in the task Layers: ${taskLayers.toString}!")
    }

    // the shared Layers must NOT have a final layer
    if(sharedLayers.finalLayer.nonEmpty) {
      throw new RuntimeException(s"ERROR: did not expect final layer in the shared Layers: ${sharedLayers.toString}!")
    }

    // if the task Layers has an embedding layer, do not use any of the shared layers (start from scratch)
    if(taskLayers.initialLayer.nonEmpty) {
      return taskLayers
    }

    // if we're here, it means that the sharedLayers must have an embedding layer
    if(sharedLayers.initialLayer.isEmpty) {
      throw new RuntimeException(s"ERROR: did not find initial layer in the shared Layers: ${sharedLayers.toString}!")
    }

    val intermediateLayers = sharedLayers.intermediateLayers ++ taskLayers.intermediateLayers
    var prevOutDim = sharedLayers.initialLayer.get.outDim
    for(il <- intermediateLayers) {
      if(il.inDim != prevOutDim) {
        throw new RuntimeException(s"ERROR: the input dimension of intermediate layer ${il.toString} does not match the expected value of $prevOutDim!")
      }
      prevOutDim = il.outDim
    }

    if(intermediateLayers.isEmpty) {
      throw new RuntimeException(s"ERROR: intermediate layers must exist in the merge!")
    }

    if(taskLayers.finalLayer.get.inDim != intermediateLayers.last.outDim) {
      throw new RuntimeException(s"ERROR: the input dimension of final layer ${taskLayers.finalLayer.get.toString} does not match the expected value of ${intermediateLayers.last.outDim}!")
    }

    val merged = new Layers(
      sharedLayers.initialLayer,
      intermediateLayers,
      taskLayers.finalLayer
    )

    merged
  }

  val MAX_INTERMEDIATE_LAYERS = 10

  def loadX2i(parameters: ParameterCollection, lines: BufferedIterator[String]): Layers = {
    val byLineIntBuilder = new ByLineIntBuilder()

    val hasInitial = byLineIntBuilder.build(lines, "hasInitial")
    val initialLayer =
      if(hasInitial == 1) {
        val layer = EmbeddingLayer.load(parameters, lines)
        //println("loaded initial layer!")
        Some(layer)
      } else {
        None
      }

    val intermediateLayers = new ArrayBuffer[IntermediateLayer]()
    val intermCount = byLineIntBuilder.build(lines, "intermediateCount")
    for(_ <- 0 until intermCount) {
      val il = RnnLayer.load(parameters, lines)
      //println("loaded one intermediate layer!")
      intermediateLayers += il
    }

    val hasFinal = byLineIntBuilder.build(lines, "hasFinal")
    val finalLayer =
      if(hasFinal == 1) {
        val layer = ForwardLayer.load(parameters, lines)
        //println("loaded final layer!")
        Some(layer)
      } else {
        None
      }

    new Layers(initialLayer, intermediateLayers, finalLayer)
  }

  def predictJointly(layers: IndexedSeq[Layers],
                     sentence: AnnotatedSentence): IndexedSeq[IndexedSeq[String]] = {
    val labelsPerTask = new ArrayBuffer[IndexedSeq[String]]()

    this.synchronized { // DyNet's computation graph is a static variable, so this block must be synchronized
      ComputationGraph.renew()

      // layers(0) contains the shared layers
      if(layers(0).nonEmpty) {
        val sharedStates = layers(0).forward(sentence)

        for (i <- 1 until layers.length) {
          val states = layers(i).forwardFrom(sharedStates)
          val emissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(states)
          val labels = layers(i).finalLayer.get.inference(emissionScores)
          labelsPerTask += labels
        }
      }

      // no shared layer
      else {
        for (i <- 1 until layers.length) {
          val states = layers(i).forward(sentence)
          val emissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(states)
          val labels = layers(i).finalLayer.get.inference(emissionScores)
          labelsPerTask += labels
        }
      }
    }

    labelsPerTask
  }
}
