package org.clulab.processors.clu 

import org.clulab.scala.WrappedArray._
import org.clulab.scala.WrappedArrayBuffer._
import org.clulab.scala.WrappedListBuffer._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import EisnerEnsembleParser._

import scala.collection.mutable

/** 
  * Stores one dependency for the Eisner algorithm 
  * Indexes for head and mod start at 1 for the first word in the sentence; 0 is reserved for root
  */

class Span(val dependencies: Seq[Dependency], val head: Int, val score: Float) {
  def this() = {
    this(List[Dependency](), Dependency.ROOT, 0f)
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
    val deps = new ListBuffer[Dependency]
    val allNodes = new mutable.HashSet[Int]
    val modNodes = new mutable.HashSet[Int]
    if(dep != null) {
      addDep(dep, deps, allNodes, modNodes)
    }
    for(dep <- left.dependencies) {
      addDep(dep, deps, allNodes, modNodes)
    }
    for(dep <- right.dependencies) {
      addDep(dep, deps, allNodes, modNodes)
    }
      
    new Span(deps, head, score)
  }

  private def addDep(dep: Dependency,
                     deps: ListBuffer[Dependency],
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
    val c = Array.tabulate(dimension) { i =>
      Array.tabulate(dimension) { j =>
        if (i == j) Array(new Span(), new Span())
        else new Array[Span](2) // These are left null.
      }
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

class EisnerEnsembleParser {

  def parse(startingDependencies: Array[Array[Dependency]]): Option[Span] = {
    // for(i <- startingDependencies.indices) println(s"Index $i: " + startingDependencies(i).mkString(", "))

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

  def generateOutput(top: Span): Array[HeadLabel] = {
    Dependency.toHeadLabels(top.dependencies)
  }

  /** Converts the top K predictions from an unlabeled parser into a matrix of Dependency (rows are mods; columns are heads) */
  def toDependencyTable(sentHeadModLabelScores: Array[Array[Dependency]], topK: Int): Array[Array[Dependency]] = {
    val extension = 1 // This should probably be -HeadLabelScore.ROOT.
    val extendedSentLength = sentHeadModLabelScores.length + extension
    // WARNING: Untouched values will be null!
    val dependencies = Array.fill(extendedSentLength)(new Array[Dependency](extendedSentLength))

    sentHeadModLabelScores.foreach { wordHeadModLabelScores =>
      val bestHeadModLabelScores = wordHeadModLabelScores.take(topK)

      bestHeadModLabelScores.foreach { headModLabelScore =>
        dependencies(headModLabelScore.mod)(headModLabelScore.head) = headModLabelScore
      }
    }

    // printDependencyTable(dependencies)
    dependencies
  }

  def printDependencyTable(deps: Array[Array[Dependency]]): Unit = {
    p(s"Dependency table of length ${deps.length}:")
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
}

object EisnerEnsembleParser {
  // prints for debugging
  val DEBUG = false
  def p(s: String): Unit = { if(DEBUG) print(s) }
  def pl(s: String = ""): Unit = { if(DEBUG) println(s) }

  // types of spans in the chart
  val HEAD_LEFT = 0
  val HEAD_RIGHT = 1
}
