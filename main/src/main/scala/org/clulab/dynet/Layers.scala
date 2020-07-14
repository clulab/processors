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

  /** Forward pass until the final layer */
  protected def forward(sentence: AnnotatedSentence,
                        predicatePositions: IndexedSeq[Int],
                        doDropout: Boolean): ExpressionVector = {
    if(initialLayer.isEmpty) {
      throw new RuntimeException(s"ERROR: you can't call forward() on a Layers object that does not have an initial layer: $toString!")
    }

    var states = initialLayer.get.forward(sentence, predicatePositions, doDropout)

    for (i <- intermediateLayers.indices) {
      states = intermediateLayers(i).forward(states, doDropout)
    }

    states
  }

  /** Forward pass until the final layer, starting from a non-initial state */
  protected def forwardFrom(inStates: ExpressionVector,
                            doDropout: Boolean): ExpressionVector = {
    if(initialLayer.nonEmpty) {
      throw new RuntimeException(s"ERROR: you can't call forwardFrom() on a Layers object that has an initial layer: $toString!")
    }

    var states = inStates

    for (i <- intermediateLayers.indices) {
      states = intermediateLayers(i).forward(states, doDropout)
    }

    states
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

  private def forwardForTask(layers: IndexedSeq[Layers],
                             taskId: Int,
                             sentence: AnnotatedSentence,
                             predPositions: IndexedSeq[Int],
                             doDropout: Boolean): IndexedSeq[ExpressionVector] = {
    //
    // make sure this code is:
    //   (a) called inside a synchronized block, and
    //   (b) called after the computational graph is renewed (see predict below for correct usage)
    //

    // run until final layer
    val preFinalStates = {
      // layers(0) contains the shared layers
      if (layers(0).nonEmpty) {
        val sharedStates = layers(0).forward(sentence, predPositions, doDropout)
        layers(taskId + 1).forwardFrom(sharedStates, doDropout)
      }

      // no shared layer
      else {
        layers(taskId + 1).forward(sentence, predPositions, doDropout)
      }
    }

    // final layer, once for each predicate position
    val allFinalStates = {
      val allScores = new ArrayBuffer[ExpressionVector]()

      if (predPositions.isEmpty) {
        val scores = layers(taskId + 1).finalLayer.get.forward(preFinalStates, None, doDropout)
        allScores += scores
      } else {
        for(predPosition <- predPositions) {
          val scores = layers(taskId + 1).finalLayer.get.forward(preFinalStates, Some(predPosition), doDropout)
          allScores += scores
        }
      }

      allScores
    }

    allFinalStates
  }

  // Note: this is currently supported only for basic tasks
  def predictJointly(layers: IndexedSeq[Layers],
                     sentence: AnnotatedSentence): IndexedSeq[IndexedSeq[String]] = {
    val labelsPerTask = new ArrayBuffer[IndexedSeq[String]]()

    DyNetSync.synchronized { // DyNet's computation graph is a static variable, so this block must be synchronized
      ComputationGraph.renew()

      // layers(0) contains the shared layers
      if(layers(0).nonEmpty) {
        val sharedStates = layers(0).forward(sentence, IndexedSeq(), doDropout = false)

        for (i <- 1 until layers.length) {
          // run until final layer
          val preFinalStates = layers(i).forwardFrom(sharedStates, doDropout = false)

          // final layer scores
          val finalStates = layers(i).finalLayer.get.forward(preFinalStates, None, doDropout = false)

          // inference
          val emissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(finalStates)
          val labels = layers(i).finalLayer.get.inference(emissionScores)
          labelsPerTask += labels
        }
      }

      // no shared layer
      else {
        for (i <- 1 until layers.length) {
          // run until final layer
          val preFinalStates = layers(i).forward(sentence, IndexedSeq(), doDropout = false)

          // final layer scores
          val finalStates = layers(i).finalLayer.get.forward(preFinalStates, None, doDropout = false)

          val emissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(finalStates)
          val labels = layers(i).finalLayer.get.inference(emissionScores)
          labelsPerTask += labels
        }
      }
    }

    labelsPerTask
  }

  def predict(layers: IndexedSeq[Layers],
              taskId: Int,
              sentence: AnnotatedSentence,
              predPositions: IndexedSeq[Int] = IndexedSeq()): IndexedSeq[IndexedSeq[String]] = {
    val labelsForTask =
      DyNetSync.synchronized { // DyNet's computation graph is a static variable, so this block must be synchronized
        val allLabels = new ArrayBuffer[IndexedSeq[String]]()
        ComputationGraph.renew()

        val allStates = forwardForTask(layers, taskId, sentence, predPositions, doDropout = false)

        for(states <- allStates) {
          val emissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(states)
          allLabels += layers(taskId + 1).finalLayer.get.inference(emissionScores)
        }

        allLabels
      }

    labelsForTask
  }

  def loss(layers: IndexedSeq[Layers],
           taskId: Int,
           sentence: AnnotatedSentence,
           allGoldLabels: IndexedSeq[IndexedSeq[String]],
           predicatePositions: IndexedSeq[Int]): Expression = {
    val allStates = forwardForTask(layers, taskId, sentence, predicatePositions, doDropout = true)
    assert(allStates.length > 0)
    assert(allStates.length == allGoldLabels.length)

    val allLosses = new ExpressionVector()
    for(i <- allStates.indices) {
      val states = allStates(i)
      val goldLabels = allGoldLabels(i)
      val loss = layers(taskId + 1).finalLayer.get.loss(states, goldLabels)
      allLosses.add(loss)
    }

    Expression.sum(allLosses) / allLosses.length
  }

  def loss(layers: IndexedSeq[Layers],
           taskId: Int,
           sentence: AnnotatedSentence,
           allGoldLabels: IndexedSeq[String]): Expression = {
    loss(layers, taskId, sentence, Array(allGoldLabels), IndexedSeq())
  }

}
