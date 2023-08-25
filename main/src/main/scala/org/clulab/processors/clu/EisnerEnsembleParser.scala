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

  def generateOutput(top: Option[Span],
                     scores: Array[Array[(Int, String, Float)]],
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
      val sentLength = scores.length
      for(i <- scores.indices) {
        val bestDeps = scores(i).sortBy(- _._3) // sort from highest to lowest logits
        var foundValid = false

        for(bestDep <- bestDeps if ! foundValid) {
          val relativeHead = bestDep._1.toInt
          val label = bestDep._2
          val depMod = i + 1
          val depHead = if (relativeHead == 0) 0 else depMod + relativeHead

          // found a valid head, i.e., within the sentence boundaries or root (-1)
          if(depHead >= -1 && depHead < sentLength) {
            val head =
              if(generateRelativeHeads) {
                // we are storing *relative* head positions here
                relativeHead
              } else {
                // we are storing absolute heads, starting at offset 0
                depHead - 1
            }

            heads(i) = (head, label)
            foundValid = true
          }
        }
      }
    }
    heads
  }

  /** Converts the top K predictions from an unlabeled parserinto a matrix of Dependency (rows are mods; columns are heads) */
  def toDependencyTable(scores: Array[Array[(Int, String, Float)]],
                        topK: Int): Array[Array[Dependency]] = {
    val length = scores.length + 1 // plus 1 to accomodate for root
    val dependencies = Array.fill(length)(new Array[Dependency](length))

    for(i <- scores.indices) {
      val mod = i + 1 // offsets start at 1 in the Dependency class
      val sortedPreds = scores(i).sortBy(- _._3) // sort in descending order of scores
      val sortedProbs = sortedPreds.map(_._3) // MathUtils.softmaxFloat(sortedPreds.map(_._2))
      val sortedHeads = sortedPreds.map(_._1)
      val sortedLabels = sortedPreds.map(_._2)

      var headCount = 0
      for(j <- sortedHeads.indices if headCount < topK) {
        try {
          val relHead = sortedHeads(j).toInt
          val label = sortedLabels(j)
          val score = sortedProbs(j)
          val head = if (relHead == 0) 0 else mod + relHead // +1 offset from mod propagates in the head here
          if (head >= 0 && head < length) { // we may predict a head outside of sentence boundaries
            if(dependencies(mod)(head) == null || dependencies(mod)(head).score < score) { // there might be a dependency already with a higher score
              val dep = Dependency(mod, head, score, j, label)
              //println(s"Creating dependency: $dep")
              dependencies(mod)(head) = dep
              headCount += 1
            }
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
