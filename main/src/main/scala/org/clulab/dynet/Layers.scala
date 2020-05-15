package org.clulab.dynet

import com.typesafe.config.Config
import edu.cmu.dynet.{Expression, ParameterCollection}
import org.clulab.struct.Counter
import org.clulab.utils.Configured

import scala.collection.mutable.ArrayBuffer

/**
 * A sequence of layers that implements a complete NN architecture for sequence modeling
 */
class Layers (val initialLayer: Option[InitialLayer],
              val intermediateLayers: IndexedSeq[IntermediateLayer],
              val finalLayer: Option[FinalLayer]) {
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

  def needsPosTags: Boolean = initialLayer.nonEmpty && initialLayer.get.needsPosTags

  def loss(words: IndexedSeq[String],
           posTags: Option[IndexedSeq[String]],
           predicatePosition: Option[Int],
           goldLabels: IndexedSeq[String]): Expression = {
    assert(initialLayer.nonEmpty)
    assert(finalLayer.nonEmpty)

    var states = initialLayer.get.forward(words, posTags, predicatePosition, true)
    for(i <- intermediateLayers.indices) {
      states = intermediateLayers(i).forward(states, true)
    }
    states = finalLayer.get.forward(states, predicatePosition, true)

    finalLayer.get.loss(states, goldLabels)
  }
}

object Layers {
  def apply(config: Configured,
            paramPrefix: String,
            parameters: ParameterCollection,
            wordCounter: Counter[String],
            labelCounterOpt: Option[Counter[String]]): Layers = {
    val initialLayer = EmbeddingLayer.initialize(config, paramPrefix + ".initial", parameters, wordCounter)

    val intermediateLayers = new ArrayBuffer[IntermediateLayer]()
    var done = false
    for(i <- 1 to MAX_INTERMEDIATE_LAYERS if ! done) {
      val intermediateLayer = RnnLayer.initialize(config, paramPrefix + s".intermediate$i", parameters)
      if(intermediateLayer.nonEmpty) {
        intermediateLayers += intermediateLayer.get
      } else {
        done = true
      }
    }

    val finalLayer =
      if(labelCounterOpt.nonEmpty) {
        ForwardLayer.initialize(config, paramPrefix + ".final", parameters, labelCounterOpt.get)
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

}
