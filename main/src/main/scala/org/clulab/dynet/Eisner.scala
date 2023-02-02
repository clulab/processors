package org.clulab.dynet

import org.clulab.scala.WrappedArray._
import org.clulab.scala.WrappedArrayBuffer._
import org.clulab.utils.Buffer

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

import Eisner._

/** 
  * Stores one dependency for the Eisner algorithm 
  * Indexes for head and mod start at 1 for the first word in the sentence; 0 is reserved for root
  */
case class Dependency(mod:Int, head:Int, var score:Float, rank: Int, var label:String = "")

class Span(val dependencies: Seq[Dependency], val head: Int, val score: Float) {
  def this() = {
    this(List[Dependency](), -1, 0f)
  }

  override def toString: String = {
    val sb = new StringBuilder()
    for(dep <- dependencies) {
      sb.append(s"\t$dep\n")
    }
    sb.toString()
  }

  def contains(mod: Int, head: Int): Boolean = {
    dependencies.exists { dep => dep.mod == mod && dep.head == head }
  }

  def isEmpty: Boolean = dependencies.isEmpty
}

object Span {
  def apply(left: Span, right: Span, dep: Dependency, head: Int): Span = {
    // product of probabilities, in log space
    val score = left.score + right.score + (if(dep != null) math.log(dep.score).toFloat else 0f)

    // aggregate all dependencies for this span
    val allNodes = new mutable.HashSet[Int]
    val modNodes = new mutable.HashSet[Int]
    val deps = Buffer.makeArray[Dependency] { deps =>
      if (dep != null)
        addDep(dep, deps, allNodes, modNodes)
      for (dep <- left.dependencies)
        addDep(dep, deps, allNodes, modNodes)
      for (dep <- right.dependencies)
        addDep(dep, deps, allNodes, modNodes)
    }
      
    new Span(deps, head, score)
  }

  private def addDep(dep: Dependency,
                     deps: ArrayBuffer[Dependency],
                     allNodes: mutable.HashSet[Int],
                     modNodes: mutable.HashSet[Int]): Unit = {
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
      (1, span.score) // the first value in the tuple just indicates the type of action taken
    } else if(chart(start)(end)(spanType).score < span.score) {
      chart(start)(end)(spanType) = span
      (2, span.score)
    } else {
      (0, span.score)
    }
  }

  override def toString: String = {
    val sb = new StringBuilder()
    for(mod <- 0 until dimension) {
      for(head <- 0 until dimension) {
        for(spanType <- 0 until 2) {
          val spanTypeAsString = if(spanType == HEAD_LEFT) "left" else "right"
          val span = chart(mod)(head)(spanType)
          if(span != null && ! span.isEmpty) {
            sb.append(s"[$mod -- $head] (head $spanTypeAsString)\n")
            sb.append(span)
          }
        }
      }
    }

    sb.toString()
  }
}

class Eisner {
  def parse(startingDependencies: Array[Array[Dependency]]): Option[Span] = {
    val length = startingDependencies.length
    val chart = new Chart(length)

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
            if(d != null) {
              chart.set(start, end, HEAD_RIGHT, Span(ll, rr, d, rr.head))  
            }

            // merge [start(m), split] and [split + 1(h), end]
            d = startingDependencies(start)(split + 1)
            if(d != null) {
              chart.set(start, end, HEAD_RIGHT, Span(ll, rr, d, rr.head))
            }

            // merge [start(h), split] and [split + 1, end(m)]
            d = startingDependencies(end)(start)
            if(d != null) { 
              chart.set(start, end, HEAD_LEFT, Span(ll, rr, d, ll.head))
            }

            // merge [start, split(h)] and [split + 1, end(m)]
            d = startingDependencies(end)(split)
            if(d != null) {
              chart.set(start, end, HEAD_LEFT, Span(ll, rr, d, ll.head))
            }
          }

          val lr = chart.get(start, split, HEAD_RIGHT)
          if(lr != null && rr != null) {
            // merge [start, split(m)] and [split + 1(h), end]
            var d = startingDependencies(split)(split + 1)
            if(d != null) {
              chart.set(start, end, HEAD_RIGHT, Span(lr, rr, d, rr.head))
            }

            // merge [start, split(m)] and [split + 1, end(h)]
            d = startingDependencies(split)(end)
            if(d != null) {
              chart.set(start, end, HEAD_RIGHT, Span(lr, rr, d, rr.head))
            }
          }

          val rl = chart.get(split + 1, end, HEAD_LEFT)
          if(ll != null && rl != null) {
            // merge [start, split(h)] and [split + 1(m), end]
            var d = startingDependencies(split + 1)(split)
            if(d != null) {
              chart.set(start, end, HEAD_LEFT, Span(ll, rl, d, ll.head))
            }

            // merge [start(h), split] and [split + 1(m), end]
            d = startingDependencies(split + 1)(start)
            if(d != null) {
              chart.set(start, end, HEAD_LEFT, Span(ll, rl, d, ll.head))
            }
          }

          //
          // merge [start, split] and [split, end] in both directions
          //
          val leftRightComplete = chart.get(start, split, HEAD_RIGHT)
          val leftLeftIncomplete = chart.get(start, split, HEAD_LEFT)
          val rightRightIncomplete = chart.get(split, end, HEAD_RIGHT)
          val rightLeftComplete = chart.get(split, end, HEAD_LEFT)

          // merge [start(h), split] and [split(h), end]
          if(leftLeftIncomplete != null && rightLeftComplete != null) {
            chart.set(start, end, HEAD_LEFT, 
              Span(leftLeftIncomplete, rightLeftComplete, null, leftLeftIncomplete.head))
          }
          // merge [start, split(h)] and [split, end(h)]
          if(leftRightComplete != null && rightRightIncomplete != null) {
            chart.set(start, end, HEAD_RIGHT, 
              Span(leftRightComplete, rightRightIncomplete, null, rightRightIncomplete.head))
          }

        }
      }

      //pl(s"Chart after spanLen = $spanLen")
      //pl(chart.toString())
    }

    val top = chart.get(0, length - 1, HEAD_LEFT)
    Option(top)
  }

  def generateOutput(top: Option[Span],
                     scores: IndexedSeq[IndexedSeq[(String, Float)]],
                     dependencies: Array[Array[Dependency]],
                     generateRelativeHeads: Boolean): IndexedSeq[(Int, String)] = {

    val heads = new Array[(Int, String)](scores.size)
    if(top.nonEmpty) {
      // Eisner correctly produced a full tree
      for(dep <- top.get.dependencies) {
        val head =
          if(generateRelativeHeads) {
            // we are storing *relative* head positions here
            if (dep.head == 0) 0 else dep.head - dep.mod
          } else {
            // we are storing absolute heads, starting at offset 0
            dep.head - 1
          }
        val label = dep.label
        heads(dep.mod - 1) = (head, label)
      }
    } else {
      // Eisner failed to produce a complete tree; revert to the greedy inference
      for(i <- scores.indices) {
        val relativeHead = scores(i).maxBy(_._2)._1.toInt
        val depMod = i + 1
        val depHead = if (relativeHead == 0) 0 else depMod + relativeHead
        // lift() checks the index, and Option(_) checks for nulls.
        val valid = dependencies(depMod).lift(depHead).flatMap(Option(_)).isDefined
        val label = if (valid) dependencies(depMod)(depHead).label else "root"
        val head =
          if(generateRelativeHeads) {
            // we are storing *relative* head positions here
            if (valid) relativeHead else 0
          } else {
            // we are storing absolute heads, starting at offset 0
            if (valid) depHead - 1 else -1
          }
        heads(i) = (head, label)
      }
    }
    heads
  }

  /** Converts the top K predictions from an unlabeled parserinto a matrix of Dependency (rows are mods; columns are heads) */
  def toDependencyTable(scores: IndexedSeq[IndexedSeq[(String, Float)]],
                        topK: Int): Array[Array[Dependency]] = {
    val length = scores.length + 1 // plus 1 to accomodate for root
    val dependencies = Array.fill(length)(new Array[Dependency](length))

    for(i <- scores.indices) {
      val mod = i + 1 // offsets start at 1 in the Dependency class
      val sortedPreds = scores(i).sortBy(- _._2)
      val sortedProbs = sortedPreds.map(_._2) // MathUtils.softmaxFloat(sortedPreds.map(_._2))
      val sortedLabels = sortedPreds.map(_._1)
      //println(s"SORTED LABELS for ${i}: $sortedLabels")
      var headCount = 0
      for(j <- sortedLabels.indices if headCount < topK) {
        try {
          val relHead = sortedLabels(j).toInt
          val score = sortedProbs(j)
          //println(s"Converting mod $mod and relHead $relHead")
          val head = if (relHead == 0) 0 else mod + relHead // +1 offset from mod propagates in the head here
          if (head >= 0 && head < length) { // we may predict a head outside of sentence boundaries
            //println(s"Added dependency: $mod $head $score")
            dependencies(mod)(head) = Dependency(mod, head, score, j)
            headCount += 1
          }
        } catch {
          case _: NumberFormatException => // Ok to skip these, since the parser may predict <START> and <STOP> labels
        }
      }
    }

    //printDependencyTable(dependencies)
    dependencies
  }

  def printDependencyTable(deps: Array[Array[Dependency]]): Unit = {
    for(i <- deps.indices) {
      p(s"$i:")
      for(j <- deps(i).indices) {
        val dep = deps(i)(j)
        p("\t")
        if(dep != null) p(dep.score.toString)
        else p("-")
      }
      pl()
    }
  }

  /** Eisner algorithm using as dependency score the head score + label score */
  def ensembleParser(mtlHeads: Metal, mtlLabels: Option[Metal], sentence: AnnotatedSentence,
                     constEmbeddings: ConstEmbeddingParameters,
                     topK: Int,
                     lambda: Float,
                     generateRelativeHeads: Boolean): IndexedSeq[(Int, String)] = {

    // construct the dependency table using just the head prediction scores
    val scores = mtlHeads.predictWithScores(0, sentence, None, constEmbeddings)
    val startingDependencies = toDependencyTable(scores, topK) // currently the score of a dependency is just the head score

    //
    // add label scores to all dependencies
    //
    if(mtlLabels.nonEmpty) {
      // prepare the (modifier, head) pairs for which we will get label scores
      val modHeadPairs = Buffer.makeArray[ModifierHeadPair] { modHeadPairs =>
        for (deps <- startingDependencies; dep <- deps; if dep != null)
          modHeadPairs += ModifierHeadPair(dep.mod - 1, dep.head - 1) // out offsets start at 0 not at 1 as in Dependency
      }

      // generate label probabilities using the label classifier
      val labelScores =
        mtlLabels.get.predictWithScores(0, sentence, Some(modHeadPairs), constEmbeddings) // these are probs
      val labelTopScores =
        labelScores.map(x => x.filterNot{case (v, _) => v == Utils.STOP_TAG}.maxBy(_._2)) // keep just the top score for each label that is not STOP
      assert(labelTopScores.size == modHeadPairs.size)

      // linearly interpolate the head and label scores in the dependency table
      for(i <- labelTopScores.indices) {
        val topLabelAndScore = labelTopScores(i)
        val modHeadPair = modHeadPairs(i)
        val mod = modHeadPair.modifier
        val head = modHeadPair.head

        //println(s"lambda = $lambda")
        //println(s"head score = ${startingDependencies(mod + 1)(head + 1).score}")
        //println(s"label score = ${topLabelAndScore._2}\n")

        startingDependencies(mod + 1)(head + 1).score =
          lambda * startingDependencies(mod + 1)(head + 1).score + (1 - lambda) * topLabelAndScore._2
        startingDependencies(mod + 1)(head + 1).label =
          topLabelAndScore._1
      }

      // another heuristic: more complicated, works worse
      /*
      // linearly interpolate the head and label scores in the dependency table
      for(i <- labelScores.indices) {
        val sortedLabels = labelScores(i).sortBy(- _._2)
        val modHeadPair = modHeadPairs(i)
        val mod = modHeadPair.modifier + 1
        val head = modHeadPair.head + 1

        // if (rank != 0 && STOP) discard
        // else if(STOP) pick second
        // else pick first
        if(startingDependencies(mod)(head).rank != 0 && sortedLabels.head._1 == Utils.STOP_TAG) {
          startingDependencies(mod)(head) == null
        } else if(sortedLabels.head._1 == Utils.STOP_TAG) {
          val secondLabel = sortedLabels(1)
          startingDependencies(mod)(head).score =
            lambda * startingDependencies(mod)(head).score + (1 - lambda) * secondLabel._2
          startingDependencies(mod)(head).label =
            secondLabel._1
        } else {
          val firstLabel = sortedLabels.head
          startingDependencies(mod)(head).score =
            lambda * startingDependencies(mod)(head).score + (1 - lambda) * firstLabel._2
          startingDependencies(mod)(head).label =
            firstLabel._1
        }
      }
      */
    }

    // the actual Eisner parsing algorithm
    val top = parse(startingDependencies)

    // convert back to relative (or absolute) heads
    generateOutput(top, scores, startingDependencies, generateRelativeHeads)
  }

}

object Eisner {
  // prints for debugging
  val DEBUG = false
  def p(s: String): Unit = { if(DEBUG) print(s) }
  def pl(s: String = ""): Unit = { if(DEBUG) println(s) }

  // types of spans in the chart
  val HEAD_LEFT = 0
  val HEAD_RIGHT = 1
}
