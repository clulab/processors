package org.clulab.dynet

import java.io.PrintWriter
import edu.cmu.dynet.{Expression, ExpressionVector, ParameterCollection}
import org.clulab.struct.Counter
import org.clulab.utils.Configured
import org.clulab.dynet.Utils._
import org.clulab.fatdynet.utils.Synchronizer

import scala.collection.mutable.ArrayBuffer
import org.clulab.utils.MathUtils

import org.clulab.dynet.Layers.Chart._
import scala.concurrent.duration.span
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet

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

  protected def forward(sentence: AnnotatedSentence,
                        constEmbeddings: ConstEmbeddingParameters,
                        doDropout: Boolean): ExpressionVector = {
    if(initialLayer.isEmpty) {
      throw new RuntimeException(s"ERROR: you can't call forward() on a Layers object that does not have an initial layer: $toString!")
    }

    var states = initialLayer.get.forward(sentence, constEmbeddings, doDropout)

    for (i <- intermediateLayers.indices) {
      states = intermediateLayers(i).forward(states, doDropout)
    }

    if(finalLayer.nonEmpty) {
      states = finalLayer.get.forward(states, sentence.headPositions, doDropout)
    }

    states
  }

  protected def forwardFrom(inStates: ExpressionVector,
                            headPositions: Option[IndexedSeq[Int]],
                            doDropout: Boolean): ExpressionVector = {
    if(initialLayer.nonEmpty) {
      throw new RuntimeException(s"ERROR: you can't call forwardFrom() on a Layers object that has an initial layer: $toString!")
    }

    var states = inStates

    for (i <- intermediateLayers.indices) {
      states = intermediateLayers(i).forward(states, doDropout)
    }

    if(finalLayer.nonEmpty) {
      states = finalLayer.get.forward(states, headPositions, doDropout)
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
            isDual: Boolean,
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
          labelCounterOpt.get, isDual, inputSize.get)
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

  def predictJointly(layers: IndexedSeq[Layers],
                     sentence: AnnotatedSentence,
                     constEmbeddings: ConstEmbeddingParameters): IndexedSeq[IndexedSeq[String]] = {
    val labelsPerTask = new ArrayBuffer[IndexedSeq[String]]()

    // DyNet's computation graph is a static variable, so this block must be synchronized
    Synchronizer.withComputationGraph("Layers.predictJointly()") {
      // layers(0) contains the shared layers
      if (layers(0).nonEmpty) {
        val sharedStates = layers(0).forward(sentence, constEmbeddings, doDropout = false)

        for (i <- 1 until layers.length) {
          val states = layers(i).forwardFrom(sharedStates, sentence.headPositions, doDropout = false)
          val emissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(states)
          val labels = layers(i).finalLayer.get.inference(emissionScores)
          labelsPerTask += labels
        }
      }
      // no shared layer
      else {
        for (i <- 1 until layers.length) {
          val states = layers(i).forward(sentence, constEmbeddings, doDropout = false)
          val emissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(states)
          val labels = layers(i).finalLayer.get.inference(emissionScores)
          labelsPerTask += labels
        }
      }
    }

    labelsPerTask
  }

  private def forwardForTask(layers: IndexedSeq[Layers],
                             taskId: Int,
                             sentence: AnnotatedSentence,
                             constEmbeddings: ConstEmbeddingParameters,
                             doDropout: Boolean): ExpressionVector = {
    //
    // make sure this code is:
    //   (a) called inside a synchronized block, and
    //   (b) called after the computational graph is renewed (see predict below for correct usage)
    //

    val states = {
      // layers(0) contains the shared layers
      if (layers(0).nonEmpty) {
        val sharedStates = layers(0).forward(sentence, constEmbeddings, doDropout)
        layers(taskId + 1).forwardFrom(sharedStates, sentence.headPositions, doDropout)
      }

      // no shared layer
      else {
        layers(taskId + 1).forward(sentence, constEmbeddings, doDropout)
      }
    }

    states
  }

  def predict(layers: IndexedSeq[Layers],
              taskId: Int,
              sentence: AnnotatedSentence,
              constEmbeddings: ConstEmbeddingParameters): IndexedSeq[String] = {
    val labelsForTask =
      // DyNet's computation graph is a static variable, so this block must be synchronized.
      Synchronizer.withComputationGraph("Layers.predict()") {
        val states = forwardForTask(layers, taskId, sentence, constEmbeddings, doDropout = false)
        val emissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(states)
        val out = layers(taskId + 1).finalLayer.get.inference(emissionScores)

        out
      }

    labelsForTask
  }

  def predictWithScores(layers: IndexedSeq[Layers],
                        taskId: Int,
                        sentence: AnnotatedSentence,
                        constEmbeddings: ConstEmbeddingParameters): IndexedSeq[IndexedSeq[(String, Float)]] = {
    val labelsForTask =
      // DyNet's computation graph is a static variable, so this block must be synchronized
      Synchronizer.withComputationGraph("Layers.predictWithScores()") {
        val states = forwardForTask(layers, taskId, sentence, constEmbeddings, doDropout = false)
        val emissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(states)
        val out = layers(taskId + 1).finalLayer.get.inferenceWithScores(emissionScores)

        out
      }

    labelsForTask
  }

  /** 
    * Stores one dependency for the Eisner algorithm 
    * Indexes for head and mod start at 1 for the first word in the sentence; 0 is reserved for root
    */
  case class Dependency(mod:Int, head:Int, score:Float)

  /** Converts the top K predictions into a matrix of Dependency (rows are mods; columns are heads) */
  private def toDependencyTable(scores: IndexedSeq[IndexedSeq[(String, Float)]], topK: Int): Array[Array[Dependency]] = {
    val length = scores.length + 1 // plus 1 to accomodate for root
    val dependencies = Array.fill(length)(new Array[Dependency](length))

    for(i <- scores.indices) {
      val mod = i + 1 // offsets start at 1 in Dependency
      val sortedPreds = scores(i).sortBy(- _._2)
      val sortedProbs = MathUtils.softmaxFloat(sortedPreds.map(_._2), 1f)
      val sortedLabels = sortedPreds.map(_._1)
      for(j <- 0 until math.min(topK, sortedPreds.size)) {
        val relHead = sortedLabels(j).toInt
        val score = sortedProbs(j)
        //println(s"Converting mod $mod and relHead $relHead")
        val head = if(relHead == 0) 0 else mod + relHead // +1 offset from mod propagates in the head here
        if(head >= 0 && head < length) { // we may predict a head outside of sentence boundaries
          dependencies(mod)(head) = Dependency(mod, head, score.toFloat)
        }
      }
    }

    dependencies
  }

  val DEBUG = true

  private def p(s: String) { if(DEBUG) print(s) }
  private def pl(s: String = "") { if(DEBUG) println(s) }

  private def printDependencyTable(deps: Array[Array[Dependency]]) {
    for(i <- deps.indices) {
      p(s"$i:")
      for(j <- deps(i).indices) {
        val dep = deps(i)(j)
        p("\t")
        if(dep != null) p(dep.score.toString())
        else p("-")
      }
      pl()
    }
  }

  def canMerge(left: Span, right: Span, dep: Dependency, headType: Int): Boolean = {
    // dep is null only when we merge adjacent spans
    // in this case, the only constraint is that the span containing the head can't be empty
    if(dep == null) {
      if(headType == HEAD_LEFT) {
        return ! left.isEmpty
      } else if(headType == HEAD_RIGHT) {
        return ! right.isEmpty
      } else {
        return false
      }
    }

    //
    // if headType is LEFT, it means the head of the merged span will be in the left span, 
    //   which means the head of the right span must be the modifier of the dependency
    //
    if(headType == HEAD_LEFT) {
      if(right.isEmpty) {
        return true
      } else if(right.head == dep.mod) {
        return true
      } else {
        return false
      }
    }

    //
    // if headType is RIGHT, it means the head of the merged span will be in the right span, 
    //   which means the head of the left span must be the modifier of the dependency
    //
    else if(headType == HEAD_RIGHT) {
      if(left.isEmpty) {
        return true
      } else if(left.head == dep.mod) {
        return true
      } else {
        return false
      }
    }

    false
  }

  def parseFromTopK(layers: IndexedSeq[Layers],
                    taskId: Int,
                    sentence: AnnotatedSentence,
                    constEmbeddings: ConstEmbeddingParameters,
                    topK: Int): IndexedSeq[Int] = {
    val scores = predictWithScores(layers, taskId, sentence, constEmbeddings)
    val startingDependencies = toDependencyTable(scores, topK)
    printDependencyTable(startingDependencies)
    val length = startingDependencies.length
    val chart = new Chart(length)

    TOTAL += 1

    val START_CHECK = 2
    val END_CHECK = 25
    val TYPE_CHECK = HEAD_RIGHT

    for(spanLen <- 2 to length) {
      for(start <- 0 to length - spanLen) {
        val end = start + spanLen - 1 // inclusive
        pl(s"Span: [$start, $end]")
        for(split <- start until end) {

          val ll = chart.get(start, split, HEAD_LEFT)
          val rr = chart.get(split + 1, end, HEAD_RIGHT)
          if(ll != null && rr != null) {
            // merge [start(m), split] and [split + 1, end(h)]
            var d = startingDependencies(start)(end)
            if(d != null && canMerge(ll, rr, d, HEAD_RIGHT)) {
              val r = chart.set(start, end, HEAD_RIGHT, Span(ll, rr, d, rr.head))  
              if(start == START_CHECK && end == END_CHECK && TYPE_CHECK == HEAD_RIGHT && r._1 != 0) {
                println(s"Creating RIGHT span from $start to $end with score ${r._2} using:")
                println(s"\tLeft: $start, $split, LEFT")
                println(ll)
                println(s"\tRight: ${split + 1}, $end, RIGHT")
                println(rr)
                println("\t" + d)
              }
            }

            // merge [start(m), split] and [split + 1(h), end]
            d = startingDependencies(start)(split + 1)
            if(d != null && canMerge(ll, rr, d, HEAD_RIGHT)) {
              val r = chart.set(start, end, HEAD_RIGHT, Span(ll, rr, d, rr.head))
              if(start == START_CHECK && end == END_CHECK && TYPE_CHECK == HEAD_RIGHT && r._1 != 0) {
                println(s"Creating RIGHT span from $start to $end with score ${r._2} using:")
                println(s"\tLeft: $start, $split, LEFT")
                println(ll)
                println(s"\tRight: ${split + 1}, $end, RIGHT")
                println(rr)
                println("\t" + d)
              }
            }

            // merge [start(h), split] and [split + 1, end(m)]
            d = startingDependencies(end)(start)
            if(d != null && canMerge(ll, rr, d, HEAD_LEFT)) {
              val r = chart.set(start, end, HEAD_LEFT, Span(ll, rr, d, ll.head))
              if(start == START_CHECK && end == END_CHECK && TYPE_CHECK == HEAD_LEFT && r._1 != 0) {
                println(s"Creating LEFT span from $start to $end with score ${r._2} using:")
                println(s"\tLeft: $start, $split, LEFT")
                println(ll)
                println(s"\tRight: ${split + 1}, $end, RIGHT")
                println(rr)
                println("\t" + d)
              }
              
            }

            // merge [start, split(h)] and [split + 1, end(m)]
            d = startingDependencies(end)(split)
            if(d != null && canMerge(ll, rr, d, HEAD_LEFT)) {
              val r = chart.set(start, end, HEAD_LEFT, Span(ll, rr, d, ll.head))
              if(start == START_CHECK && end == END_CHECK && TYPE_CHECK == HEAD_LEFT && r._1 != 0) {
                println(s"Creating LEFT span from $start to $end with score ${r._2} using:")
                println(s"\tLeft: $start, $split, LEFT")
                println(ll)
                println(s"\tRight: ${split + 1}, $end, RIGHT")
                println(rr)
                println("\t" + d)
              }
              
            }
          }

          val lr = chart.get(start, split, HEAD_RIGHT)
          if(lr != null && rr != null) {
            // merge [start, split(m)] and [split + 1(h), end]
            var d = startingDependencies(split)(split + 1)
            if(d != null && canMerge(lr, rr, d, HEAD_RIGHT)) {
              val r = chart.set(start, end, HEAD_RIGHT, Span(lr, rr, d, rr.head))
              if(start == START_CHECK && end == END_CHECK && TYPE_CHECK == HEAD_RIGHT && r._1 != 0) {
                println(s"Creating RIGHT span from $start to $end with score ${r._2} using:")
                println(s"\tLeft: $start, $split, RIGHT")
                println(lr)
                println(s"\tRight: ${split + 1}, $end, RIGHT")
                println(rr)
                println("\t" + d)
              }
              
            }

            // merge [start, split(m)] and [split + 1, end(h)]
            d = startingDependencies(split)(end)
            if(d != null && canMerge(lr, rr, d, HEAD_RIGHT)) {
              val r = chart.set(start, end, HEAD_RIGHT, Span(lr, rr, d, rr.head))
              if(start == START_CHECK && end == END_CHECK && TYPE_CHECK == HEAD_RIGHT && r._1 != 0) {
                println(s"Creating RIGHT span from $start to $end with score ${r._2} using:")
                println(s"\tLeft: $start, $split, RIGHT")
                println(lr)
                println(s"\tRight: ${split + 1}, $end, RIGHT")
                println(rr)
                println("\t" + d)
              }
              
            }
          }

          val rl = chart.get(split + 1, end, HEAD_LEFT)
          if(ll != null && rl != null) {
            // merge [start, split(h)] and [split + 1(m), end]
            var d = startingDependencies(split + 1)(split)
            if(d != null && canMerge(ll, rl, d, HEAD_LEFT)) {
              val r = chart.set(start, end, HEAD_LEFT, Span(ll, rl, d, ll.head))
              if(start == START_CHECK && end == END_CHECK && TYPE_CHECK == HEAD_LEFT && r._1 != 0) {
                println(s"Creating LEFT span from $start to $end with score ${r._2} using:")
                println(s"\tLeft: $start, $split, LEFT")
                println(ll)
                println(s"\tRight: ${split + 1}, $end, LEFT")
                println(rl)
                println("\t" + d)
              }
              
            }

            // merge [start(h), split] and [split + 1(m), end]
            d = startingDependencies(split + 1)(start)
            if(d != null && canMerge(ll, rl, d, HEAD_LEFT)) {
              val r = chart.set(start, end, HEAD_LEFT, Span(ll, rl, d, ll.head))
              if(start == START_CHECK && end == END_CHECK && TYPE_CHECK == HEAD_LEFT && r._1 != 0) {
                println(s"Creating LEFT span from $start to $end with score ${r._2} using:")
                println(s"\tLeft: $start, $split, LEFT")
                println(ll)
                println(s"\tRight: ${split + 1}, $end, LEFT")
                println(rl)
                println("\t" + d)
              }
              
            }
          }

          //
          // merge [start, split] and [split, end] in both directions
          //
          val rl2 = chart.get(split, end, HEAD_LEFT)
          val rr2 = chart.get(split, end, HEAD_RIGHT)

          // merge [start(h), split] and [split(h), end]
          if(ll != null && rl2 != null && canMerge(ll, rl2, null, HEAD_LEFT)) {
            val r = chart.set(start, end, HEAD_LEFT, Span(ll, rl2, null, ll.head))
            if(start == START_CHECK && end == END_CHECK && TYPE_CHECK == HEAD_LEFT && r._1 != 0) {
                println(s"Creating LEFT span from $start to $end with score ${r._2} using:")
                println(s"\tLeft: $start, $split, LEFT")
                println(ll)
                println(s"\tRight: $split, $end, LEFT")
                println(rl2)
              }
            
          }
          // merge [start, split(h)] and [split, end(h)]
          if(lr != null && rr2 != null && canMerge(lr, rr2, null, HEAD_RIGHT)) {
            val r = chart.set(start, end, HEAD_RIGHT, Span(lr, rr2, null, rr2.head))
            if(start == START_CHECK && end == END_CHECK && TYPE_CHECK == HEAD_RIGHT && r._1 != 0) {
                println(s"Creating RIGHT span from $start to $end with score ${r._2} using:")
                println(s"\tLeft: $start, $split, RIGHT")
                println(ll)
                println(s"\tRight: $split, $end, RIGHT")
                println(rr2)
              }            
          }
          // this is illegal, the head must be L or R: merge [start, split(h)] and [split(h), end]
          /*
          if(lr != null && rl2 != null && canMerge(lr, rl2, null, HEAD_LEFT)) {
            val r = chart.set(start, end, HEAD_LEFT, Span(lr, rl2, null, lr.head))
          }
          */
          // this is illegal due to two heads: merge [start(h), split] and [split, end(h)]

        }
      }

      pl(s"Chart after spanLen = $spanLen")
      pl(chart.toString(spanLen))
      chart.sanityCheck(spanLen)
    }

    val top = chart.get(0, length - 1, HEAD_LEFT)
    if(top != null) {
      EISNER += 1
      pl("Final span:")
      pl(top.toString())

      val heads = new Array[Int](sentence.size)
      for(dep <- top.dependencies) {
        pl(s"Converting dependency: $dep")
        val label = if(dep.head == 0) 0 else (dep.head - dep.mod)
        pl(s"\tlabel = $label")
        heads(dep.mod - 1) = label
      }

      heads                      
    } else {
      val heads = new Array[Int](sentence.size)
      for(i <- scores.indices) {
        val topPred = scores(i).sortBy(- _._2).head._1
        heads(i) = topPred.toInt
      }
      heads
    }
  }

  class Span(val dependencies: Seq[Dependency], val head: Int, val score: Float) {
    def this() {
      this(List[Dependency](), -1, 0f)
    }

    override def toString(): String = {
      val sb = new StringBuilder()
      for(dep <- dependencies) {
        sb.append(s"\t$dep\n")
      }
      sb.toString()
    }

    def contains(mod: Int, head: Int): Boolean = {
      for(dep <- dependencies) {
        if(dep.mod == mod && dep.head == head) {
          return true
        }
      }
      false
    }

    def isEmpty: Boolean = dependencies.size == 0
  }

  object Span {
    def apply(left: Span, right: Span, dep: Dependency, head: Int): Span = {
      // product of probabilities, in log space
      val score = left.score + right.score + (if(dep != null) math.log(dep.score).toFloat else 0f)

      // aggregate all dependencies for this span
      val deps = new ListBuffer[Dependency]
      val allNodes = new HashSet[Int]
      val modNodes = new HashSet[Int]
      if(dep != null) {
        addDep(dep, deps, allNodes, modNodes)
      }
      for(dep <- left.dependencies) {
        addDep(dep, deps, allNodes, modNodes)
      }
      for(dep <- right.dependencies) {
        addDep(dep, deps, allNodes, modNodes)
      }
      
      // position of the head word in the sentence
      /*
      assert(allNodes.size == modNodes.size + 1)
      val head = (allNodes -- modNodes).head
      */

      new Span(deps, head, score)
    }

    private def addDep(dep: Dependency, deps: ListBuffer[Dependency], allNodes: HashSet[Int], modNodes: HashSet[Int]) {
      deps += dep
      allNodes += dep.head
      allNodes += dep.mod
      modNodes += dep.mod
    }
  }

  class Chart(val dimension: Int) {
    val chart: Array[Array[Array[Span]]] = mkChart()

    private def mkChart(): Array[Array[Array[Span]]] = {
      val c = Array.fill(dimension)(new Array[Array[Span]](dimension))
      for(i <- c.indices) {
        for(j <- c(0).indices) {
          c(i)(j) = new Array[Span](2)
        }
      }
      for(i <- c.indices) {
        c(i)(i)(HEAD_LEFT) = new Span()
        c(i)(i)(HEAD_RIGHT) = new Span()
      }
      c
    }

    def get(start: Int, end: Int, spanType: Int): Span = {
      chart(start)(end)(spanType)
    }

    def set(start: Int, end: Int, spanType: Int, span: Span): (Int, Float) = {
      if(chart(start)(end)(spanType) == null) {
        chart(start)(end)(spanType) = span
        return (1, span.score)
      } else if(chart(start)(end)(spanType).score < span.score) {
        chart(start)(end)(spanType) = span
        return (2, span.score)
      } else {
        return (0, span.score)
      }
    }

    def toString(length: Int): String = {
      val sb = new StringBuilder();
      for(mod <- 0 until dimension) {
        for(head <- 0 until dimension) { // if head - mod == length) {
          var span = chart(mod)(head)(HEAD_LEFT)
          if(span != null && ! span.isEmpty) {
            sb.append(s"[$mod -- $head] (head left)\n")
            sb.append(span)
          }
          span = chart(mod)(head)(HEAD_RIGHT)
          if(span != null && ! span.isEmpty) {
            sb.append(s"[$mod -- $head] (head right)\n")
            sb.append(span)
          }
        }
      }

      sb.toString()
    }

    def sanityCheck(spanLen: Int) {
      println(s"SANITY CHECK AFTER len = $spanLen")
      for(mod <- 0 until dimension) {
        if(mod == 0 || mod == 1) {
          var found = false
          for(head <- dimension - 1 until 0 by -1 if ! found) {
            if(chart(mod)(head)(HEAD_LEFT) != null) {
              found = true
              if(chart(mod)(head)(HEAD_LEFT).contains(7, 5)) {
                println(s"\tSpan $mod to $head contains dependency from 7 to 5")
              } else {
                println(s"\tSpan $mod to $head does NOT contain dependency from 7 to 5")
              }
            }
          }
        }

        for(head <- dimension - 1 until 0 by -1) {
          if(chart(mod)(head)(HEAD_LEFT) != null && chart(mod)(head)(HEAD_LEFT).contains(7, 5)) {
            println(s"The above chart contains dependency from 7 to 5 in Span($mod, $head, LEFT)")
          }
          if(chart(mod)(head)(HEAD_RIGHT) != null && chart(mod)(head)(HEAD_RIGHT).contains(7, 5)) {
            println(s"The above chart contains dependency from 7 to 5 in Span($mod, $head, RIGHT)")
          }
        }
      }
    }
  }

  object Chart {
    val HEAD_LEFT = 0
    val HEAD_RIGHT = 1
  }

  def parse(layers: IndexedSeq[Layers],
            sentence: AnnotatedSentence,
            constEmbeddings: ConstEmbeddingParameters): IndexedSeq[(Int, String)] = {
    val headsAndLabels =
      // DyNet's computation graph is a static variable, so this block must be synchronized
      Synchronizer.withComputationGraph("Layers.parse()") {
        //
        // first get the output of the layers that are shared between the two tasks
        //
        assert(layers(0).nonEmpty)
        val sharedStates = layers(0).forward(sentence, constEmbeddings, doDropout = false)

        //
        // now predict the heads (first task)
        //
        val headStates = layers(1).forwardFrom(sharedStates, None, doDropout = false)
        val headEmissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(headStates)
        val headScores = layers(1).finalLayer.get.inferenceWithScores(headEmissionScores)

        // store the head values here
        val heads = new ArrayBuffer[Int]()
        for(wi <- headScores.indices) {
          val predictionsForThisWord = headScores(wi)

          // pick the prediction with the highest score, which is within the boundaries of the current sentence
          var done = false
          for(hi <- predictionsForThisWord.indices if ! done) {
            try {
              val relativeHead = predictionsForThisWord(hi)._1.toInt
              if (relativeHead == 0) { // this is the root
                heads += -1
                done = true
              } else {
                val headPosition = wi + relativeHead
                if (headPosition >= 0 && headPosition < sentence.size) {
                  heads += headPosition
                  done = true
                }
              }
            } catch {
              // some valid predictions may not be integers, e.g., "<STOP>" may be predicted by the sequence model
              case e: NumberFormatException => done = false
            }
          }
          if(! done) {
            // we should not be here, but let's be safe
            // if nothing good was found, assume root
            heads += -1
          }
        }

        //
        // next, predict the labels using the predicted heads
        //
        val labelStates = layers(2).forwardFrom(sharedStates, Some(heads), doDropout = false)
        val emissionScores: Array[Array[Float]] = Utils.emissionScoresToArrays(labelStates)
        val labels = layers(2).finalLayer.get.inference(emissionScores)
        assert(labels.size == heads.size)

        heads.zip(labels)
      }

    headsAndLabels
  }

  def loss(layers: IndexedSeq[Layers],
           taskId: Int,
           sentence: AnnotatedSentence,
           goldLabels: IndexedSeq[String]): Expression = {
    val constEmbeddings = ConstEmbeddingsGlove.mkConstLookupParams(sentence.words)

    val states = forwardForTask(layers, taskId, sentence, constEmbeddings, doDropout = true) // use dropout during training!
    layers(taskId + 1).finalLayer.get.loss(states, goldLabels)
  }

  var TOTAL = 0
  var EISNER = 0
}

