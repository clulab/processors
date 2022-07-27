package org.clulab.dynet

import edu.cmu.dynet._
import org.clulab.dynet.ForwardLayer._
import org.clulab.dynet.Utils._

import java.io.PrintWriter
import scala.collection.BufferedIterator
import scala.collection.mutable.ArrayBuffer

class GreedyForwardLayer (parameters:ParameterCollection,
                          inputSize: Int,
                          isDual: Boolean,
                          t2i: Map[String, Int],
                          i2t: Array[String],
                          H: Parameter,
                          rootParam: Parameter,
                          distanceEmbeddingSize: Int,
                          distanceLookupParameters: Option[LookupParameter],
                          nonlinearity: Int,
                          dropoutProb: Float)
  extends ForwardLayer(parameters, inputSize, isDual, t2i, i2t, H, rootParam,
    distanceEmbeddingSize, distanceLookupParameters, nonlinearity, dropoutProb) {

  override def loss(finalStates: ExpressionVector, goldLabelStrings: IndexedSeq[Label]): Expression = {
    val goldLabels = Utils.toIds(goldLabelStrings.map(_.label), t2i)
    Utils.sentenceLossGreedy(finalStates, goldLabels)
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    save(printWriter, TYPE_GREEDY, "inferenceType")
    save(printWriter, inputSize, "inputSize")
    save(printWriter, if (isDual) 1 else 0, "isDual")
    save(printWriter, distanceEmbeddingSize, "distanceEmbeddingSize")
    save(printWriter, nonlinearity, "nonlinearity")
    save(printWriter, t2i, "t2i")
    save(printWriter, dropoutProb, "dropoutProb")
  }

  override def toString: String = {
    s"GreedyForwardLayer($inDim, $outDim)"
  }

  override def inference(emissionScores: Array[Array[Float]]): IndexedSeq[String] = {
    val labelIds = Utils.greedyPredict(emissionScores)
    labelIds.map(i2t(_)).toIndexedSeq
  }

  override def inferenceWithScores(emissionScores: Array[Array[Float]]): IndexedSeq[IndexedSeq[(String, Float)]] = {
    val labelsWithScores = new ArrayBuffer[IndexedSeq[(String, Float)]]

    for(scoresForPosition <- emissionScores) {
      val labelsAndScores = new ArrayBuffer[(String, Float)]()
      for(lid <- scoresForPosition.indices) {
        val label = i2t(lid)
        val score = scoresForPosition(lid)
        labelsAndScores += Tuple2(label, score)
      }
      labelsWithScores += labelsAndScores.sortBy(- _._2).toIndexedSeq
    }

    labelsWithScores.toIndexedSeq
  }
}

object GreedyForwardLayer {
  def load(parameters: ParameterCollection,
           x2iIterator: BufferedIterator[String]): GreedyForwardLayer = {
    //
    // load the x2i info
    //
    val byLineIntBuilder = new ByLineIntBuilder()
    val byLineFloatBuilder = new ByLineFloatBuilder()
    val byLineStringMapBuilder = new ByLineStringMapBuilder()

    val inputSize = byLineIntBuilder.build(x2iIterator, "inputSize")
    val isDualAsInt = byLineIntBuilder.build(x2iIterator, "isDual", DEFAULT_IS_DUAL)
    val isDual = isDualAsInt == 1
    val distanceEmbeddingSize = byLineIntBuilder.build(x2iIterator, "distanceEmbeddingSize", 0)
    val nonlinearity = byLineIntBuilder.build(x2iIterator, "nonlinearity", ForwardLayer.NONLIN_NONE)
    val t2i = byLineStringMapBuilder.build(x2iIterator, "t2i")
    val i2t = fromIndexToString(t2i)
    val dropoutProb = byLineFloatBuilder.build(x2iIterator, "dropoutProb", ForwardLayer.DEFAULT_DROPOUT_PROB)

    //
    // make the loadable parameters
    //
    val actualInputSize = if(isDual) 2 * inputSize + distanceEmbeddingSize else inputSize

    val H = parameters.addParameters(Dim(t2i.size, actualInputSize))
    val rootParam = parameters.addParameters(Dim(inputSize))

    val distanceLookupParameters =
      if(distanceEmbeddingSize > 0) Some(parameters.addLookupParameters(101, Dim(distanceEmbeddingSize)))
      else None

    new GreedyForwardLayer(parameters,
      inputSize, isDual, t2i, i2t, H, rootParam,
      distanceEmbeddingSize, distanceLookupParameters,
      nonlinearity, dropoutProb)
  }
}


