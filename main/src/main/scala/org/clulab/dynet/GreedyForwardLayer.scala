package org.clulab.dynet

import java.io.PrintWriter
import edu.cmu.dynet._
import org.clulab.dynet.ForwardLayer.TYPE_GREEDY
import edu.cmu.dynet.Expression.{concatenate, input, logSumExp, lookup, pick, pickNegLogSoftmax, sum}
import org.clulab.dynet.Utils.{ByLineFloatBuilder, ByLineIntBuilder, ByLineStringBuilder, ByLineStringMapBuilder, fromIndexToString, save}
import ForwardLayer._

import scala.collection.mutable.ArrayBuffer
import org.clulab.struct.EdgeMap

import org.clulab.utils.MathUtils._
import org.clulab.struct.Edge

class GreedyForwardLayer (parameters:ParameterCollection,
                          inputSize: Int,
                          taskType: Int,
                          t2i: Map[String, Int],
                          i2t: Array[String],
                          H: Parameter,
                          rootParam: Parameter,
                          positionLookupParameters: LookupParameter, 
                          span: Option[Seq[(Int, Int)]],
                          nonlinearity: Int,
                          dropoutProb: Float)
  extends ForwardLayer(parameters, inputSize, taskType, t2i, i2t, H, rootParam, positionLookupParameters, span, nonlinearity, dropoutProb) {

  override def loss(finalStates: ExpressionVector, goldLabelStrings: IndexedSeq[String]): Expression = {
    val goldLabels = Utils.toIds(goldLabelStrings, t2i)
    Utils.sentenceLossGreedy(finalStates, goldLabels)
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    save(printWriter, TYPE_GREEDY, "inferenceType")
    save(printWriter, inputSize, "inputSize")
    save(printWriter, taskType, "taskType")
    save(printWriter, span.map(spanToString).getOrElse("none"), "span")
    save(printWriter, nonlinearity, "nonlinearity")
    save(printWriter, t2i, "t2i")
    save(printWriter, dropoutProb, "dropoutProb")
  }

  override def toString: String = {
    s"GreedyForwardLayer($inDim, $outDim)"
  }

  override def inference(emissionScores: Array[Array[Float]]): IndexedSeq[String] = {
    val labelIds = Utils.greedyPredict(emissionScores)
    labelIds.map(i2t(_))
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
      labelsWithScores += labelsAndScores.sortBy(- _._2)
    }

    labelsWithScores
  }

  /**
   * Cross entropy loss for all edges in the predicted graph
   * @param predictedGraph Graph predicted through inference
   * @param goldGraph The correct graph, containing only positive edges
   * @return Sum of all cross-entropy losses, for all edges in the predicted graph
   */
  override def graphLoss(predictedGraph: EdgeMap[Expression], goldGraph: EdgeMap[String]): Expression = {
    val goldLosses = new ExpressionVector()

    /*
    println("Gold graph:")
    for(key <- goldGraph.keys.toList.sortBy(_._2)) {
      println("\t" + key + " " + goldGraph(key))
    }
    println("Pred graph:")
    for(key <- predictedGraph.keys.toList.sortBy(_._2)) {
      println("\t" + key)
    }
    */

    // one cross-entropy loss for each edge in the predicted graph
    for(key <- predictedGraph.keys) {
      val head = key._1
      val modifier = key._2
      val predScores = predictedGraph(key)

      val goldLabel: String = if(goldGraph.contains(key)) {
        //println(s"\t\t+\t$key")
        goldGraph(key)
      } else {
        //println(s"\t\t-\t$key")
        Utils.STOP_TAG // we use STOP_TAG to indicate that an edge should *not* exist between this head and modifier
      }
      val goldLabelId = t2i(goldLabel)

      goldLosses.add(pickNegLogSoftmax(predScores, goldLabelId))
    }

    //println("Press any key to continue: ")
    //scala.io.StdIn.readChar()

    //System.exit(1)
    sum(goldLosses)
  }

  override def graphForward(inputExpressions: ExpressionVector, 
                            headPositionsOpt: Option[IndexedSeq[Int]], 
                            negativesFactor: Float,                  
                            doDropout: Boolean): EdgeMap[Expression] = {
    if(doDropout) {
      assert(headPositionsOpt.nonEmpty)
      graphForwardTrain(inputExpressions, headPositionsOpt.get, negativesFactor)
    } else {
      graphForwardTest(inputExpressions)
    }                              
  }

  private def graphForwardTrain(inputExpressions: ExpressionVector, 
                                headPositions: IndexedSeq[Int], 
                                negativesFactor: Float): EdgeMap[Expression] = {
    assert(inputExpressions.size == headPositions.size)
    val edgeMap = new EdgeMap[Expression]
    for(modifier <- inputExpressions.indices) {
      // the positive example
      val head = headPositions(modifier)
      edgeMap += ((head, modifier), runForwardDual(modifier, head, inputExpressions, true))
      //println(s"+\t($head, $modifier)")

      // up to negFactor negative examples
      // need to include -1 in the range, for root
      val negs = mkRandomRange(-1, inputExpressions.size, negativesFactor.toInt, Set(head, modifier), graphRand)
      for(neg <- negs) {
        edgeMap += ((neg, modifier), runForwardDual(modifier, neg, inputExpressions, true))
        //println(s"-\t($neg, $modifier)")
      }
    } 
    edgeMap                                      
  }

  private def graphForwardTest(inputExpressions: ExpressionVector): EdgeMap[Expression] = {
    // we try all possible heads at this stage (including root, i.e., -1)
    val edgeMap = new EdgeMap[Expression]
    for(modifier <- inputExpressions.indices) {
      for(head <- -1 until inputExpressions.size if head != modifier) {
        // no dropout during testing
        edgeMap += ((head, modifier), runForwardDual(modifier, head, inputExpressions, false)) 
      }
    }
    edgeMap
  }

  /** Greedy inference: for each node pick the head with the highest score */
  override def graphInference(emissionScores: EdgeMap[Array[Float]], sentenceSize: Int): EdgeMap[String] = {
    val predGraph = new EdgeMap[String]
    val noEdgeId = t2i(Utils.STOP_TAG)
    var rootNode = -1

    //
    // gotta make sure we have 1 root
    //
    val rootPredictions = new ArrayBuffer[(Int, Int, Int, Float)] // modifier, head, label, score
    for(modifier <- 0 until sentenceSize) {
      val scores = emissionScores((-1, modifier))
      for(labelId <- scores.indices if labelId != noEdgeId) {
        rootPredictions += Tuple4(modifier, -1, labelId, scores(labelId))
      }
    }
    val sortedRootPredictions = rootPredictions.sortBy(- _._4)
    val topPrediction = sortedRootPredictions.head
    rootNode = topPrediction._1
    val predLabel = i2t(topPrediction._3)
    predGraph += ((-1, rootNode), predLabel)
    assert(rootNode != -1)

    //
    // assign a head for all tokens that are not linked to root
    //
    for(modifier <- 0 until sentenceSize if modifier != rootNode) {
      val predictions = new ArrayBuffer[(Int, Int, Float)] // head, label, score
      // head could be root, i.e., -1
      for(head <- -1 until sentenceSize if head != modifier && emissionScores.contains((head, modifier))) { 
        val scores = emissionScores((head, modifier))
        for(labelId <- scores.indices if labelId != noEdgeId) {
          predictions += Tuple3(head, labelId, scores(labelId))
        }
      }
      val sortedPredictions = predictions.sortBy(- _._3)
      /*
      println(s"Prediction for word #$modifier:")
      for(p <- sortedPredictions) 
        if(p._2 == t2i(Utils.START_TAG)) 
          println(s"\thead ${p._1} label ${p._2} score ${p._3}")
      */
      
      val topPrediction = sortedPredictions.head
      val predHead = topPrediction._1
      val predLabel = i2t(topPrediction._2)
      predGraph += ((predHead, modifier), predLabel)
    }

    predGraph
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
    val byLineStringBuilder = new ByLineStringBuilder()

    val inputSize = byLineIntBuilder.build(x2iIterator, "inputSize")
    val taskType = byLineIntBuilder.build(x2iIterator, "taskType", ForwardLayer.DEFAULT_BASIC)
    val spanValue = byLineStringBuilder.build(x2iIterator, "span", "")
    val span = if(spanValue.isEmpty || spanValue == "none") None else Some(parseSpan(spanValue, inputSize))
    val nonlinearity = byLineIntBuilder.build(x2iIterator, "nonlinearity", ForwardLayer.NONLIN_NONE)
    val t2i = byLineStringMapBuilder.build(x2iIterator, "t2i")
    val i2t = fromIndexToString(t2i)
    val dropoutProb = byLineFloatBuilder.build(x2iIterator, "dropoutProb", ForwardLayer.DEFAULT_DROPOUT_PROB)

    //
    // make the loadable parameters
    //
    //println(s"making FF ${t2i.size} x ${2 * inputSize}")
    val needsDoubleLength = ! TaskManager.isBasic(taskType)
    val actualInputSize =
      if(span.nonEmpty) {
        val len = ForwardLayer.spanLength(span.get)
        if(needsDoubleLength) 2 * len else len
      } else {
        if(needsDoubleLength) (2 * inputSize + 32) else inputSize
      }

    val H = parameters.addParameters(Dim(t2i.size, actualInputSize))
    val rootParam = parameters.addParameters(Dim(inputSize))
    val positionLookupParameters = parameters.addLookupParameters(101, Dim(32))

    new GreedyForwardLayer(parameters,
      inputSize, taskType, t2i, i2t, H, rootParam, positionLookupParameters, 
      span, nonlinearity, dropoutProb)
  }
}


