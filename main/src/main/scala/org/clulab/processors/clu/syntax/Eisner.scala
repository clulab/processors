package org.clulab.processors.clu.syntax

import org.clulab.struct.{DirectedGraph, Edge}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import Chart._

/**
  * Implements the Eisner dynamic-programmic parsing algorithm, which we use for the ensemble model
  * User: mihais
  * Date: 8/9/17
  */
class Eisner(val individualOutputs:Array[DirectedGraph[String]]) {

  def parse(): DirectedGraph[String] = parseVoting()

  /**
    * Produces an ensemble parse using the word-by-word voting scheme from Surdeanu et al. (2010)
    * Note: this works well and fast, but it does not guarantee that the output is a tree
    * @return the DirectedGraph corresponding to the ensemble parse
    */
  def parseVoting(): DirectedGraph[String] = {
    val deps = toDependencyList(individualOutputs)

    // keep only the best dependency for each modifier
    // depMap: map from modifier to Dependency
    val depMap = new mutable.HashMap[Int, Dependency]()
    for(dep <- deps) {
      if(! depMap.contains(dep.modifier)) {
        depMap += dep.modifier -> dep
      } else if(depMap(dep.modifier).score < dep.score)
        depMap(dep.modifier) = dep
    }

    toDirectedGraph(depMap.values.toList)
  }

  /**
    * Produces an ensemble parse using the Eisner (re)parsing algorithm (1996)
    * TODO: this is currently broken. Do not use.
    * @return the DirectedGraph corresponding to the ensemble parse
    */
  def parseEisner(): DirectedGraph[String] = {
    val candTable = toDependencyTable(individualOutputs)
    val length = candTable.length
    val chart = new Chart(length)

    /*
    for(i <- individualOutputs.indices) {
      println(s"Output #$i:")
      println(individualOutputs(i))
      println()
    }

    println("Dependency table:")
    for(h <- candTable.indices) {
      for(m <- candTable(h).indices) {
        println(s"[$h, $m] = ${candTable(h)(m)}")
      }
    }
    */

    for(spanLength <- 2 to length) {
      var start = 0
      //println(s"\n\nspanLength = $spanLength")
      while(start + spanLength <= length) {
        val end = start + spanLength - 1
        //println(s"Constructing chart[$start, $end]...")

        for(split <- start until end) {

          //println(s"merging [$start, $split] and [${split + 1}, $end]")

          // 1. merge [start(m), split] and [split + 1, end(h)]
          var l = chart.get(start, split, HEAD_LEFT)
          var r = chart.get(split + 1, end, HEAD_RIGHT)
          var d = candTable(start)(end)
          assert(l != null && r != null)
          if(d != null) chart.set(start, end, HEAD_RIGHT, new Span(l, r, d))

          // 2. merge [start(m), split] and [split + 1(h), end]
          l = chart.get(start, split, HEAD_LEFT)
          r = chart.get(split + 1, end, HEAD_RIGHT)
          d = candTable(start)(split + 1)
          assert(l != null && r != null)
          if(d != null) chart.set(start, end, HEAD_RIGHT, new Span(l, r, d))

          // 3. merge [start(h), split] and [split + 1, end(m)]
          l = chart.get(start, split, HEAD_LEFT)
          r = chart.get(split + 1, end, HEAD_RIGHT)
          d = candTable(end)(start)
          assert(l != null && r != null)
          if(d != null) chart.set(start, end, HEAD_LEFT, new Span(l, r, d))

          // 4. merge [start, split(h)] and [split + 1, end(m)]
          l = chart.get(start, split, HEAD_LEFT)
          r = chart.get(split + 1, end, HEAD_RIGHT)
          d = candTable(end)(split)
          assert(l != null && r != null)
          if(d != null) chart.set(start, end, HEAD_LEFT, new Span(l, r, d))

          // 5. merge [start, split(m)] and [split + 1(h), end]
          l = chart.get(start, split, HEAD_RIGHT)
          r = chart.get(split + 1, end, HEAD_RIGHT)
          d = candTable(split)(split + 1)
          assert(l != null && r != null)
          if(d != null) chart.set(start, end, HEAD_RIGHT, new Span(l, r, d))

          // 6. merge [start, split(m)] and [split + 1, end(h)]
          l = chart.get(start, split, HEAD_RIGHT)
          r = chart.get(split + 1, end, HEAD_RIGHT)
          d = candTable(split)(end)
          assert(l != null && r != null)
          if(d != null) chart.set(start, end, HEAD_RIGHT, new Span(l, r, d))

          // 7. merge [start, split(h)] and [split + 1(m), end]
          l = chart.get(start, split, HEAD_LEFT)
          r = chart.get(split + 1, end, HEAD_LEFT)
          d = candTable(split + 1)(split)
          assert(l != null && r != null)
          if(d != null) chart.set(start, end, HEAD_LEFT, new Span(l, r, d))

          // 8. merge [start(h), split] and [split + 1(m), end]
          l = chart.get(start, split, HEAD_LEFT)
          r = chart.get(split + 1, end, HEAD_LEFT)
          d = candTable(split + 1)(start)
          assert(l != null && r != null)
          if(d != null) chart.set(start, end, HEAD_LEFT, new Span(l, r, d))

          // 9. merge [start, split] and [split, end]
          l = chart.get(start, split, HEAD_LEFT)
          r = chart.get(split, end, HEAD_LEFT)
          assert(l != null && r != null)
          chart.set(start, end, HEAD_LEFT, new Span(l, r, null))

          l = chart.get(start, split, HEAD_RIGHT)
          r = chart.get(split, end, HEAD_RIGHT)
          chart.set(start, end, HEAD_RIGHT, new Span(l, r, null))
        }

        //println(s"Chart[$start, $end, head_left] = ${chart.get(start, end, HEAD_LEFT)}")
        //println(s"Chart[$start, $end, head_right] = ${chart.get(start, end, HEAD_RIGHT)}")

        start += 1
      }
    }

    val top = chart.get(0, length - 1, HEAD_LEFT)
    //println(s"top span: $top")
    toDirectedGraph(top.deps)
  }

  def toDependencyMap(individualOutputs:Array[DirectedGraph[String]]): Map[(Int, Int, String), mutable.HashSet[Int]] = {
    // compute unique dependencies, and tally the number of votes received by each
    val depMap = new mutable.HashMap[(Int, Int, String), mutable.HashSet[Int]]()
    for(model <- individualOutputs.indices) {
      // add non-root dependencies
      for(edge <- individualOutputs(model).allEdges) {
        val head = edge._1 + 1 // our offsets start at 0; the Eisner algorithm requires them to start at 1
        val mod = edge._2 + 1
        val label = edge._3
        val votes = depMap.getOrElseUpdate(Tuple3(head, mod, label), new mutable.HashSet[Int]())
        votes += model
      }

      // add dependency(ies) to root
      val roots = individualOutputs(model).roots
      for(root <- roots) {
        val votes = depMap.getOrElseUpdate(Tuple3(0, root + 1, "root"), new mutable.HashSet[Int]())
        votes += model
      }
    }

    depMap.toMap
  }

  def toDependencyList(individualOutputs:Array[DirectedGraph[String]]): List[Dependency] = {
    val depMap = toDependencyMap(individualOutputs)

    // create the actual dependency list
    val l = new ListBuffer[Dependency]
    for(depKey <- depMap.keySet) {
      val votes = depMap.get(depKey)
      assert(votes.nonEmpty)
      val dep = Dependency(depKey._1, depKey._2, depKey._3, votes.get.toSet)
      l += dep
    }

    l.toList
  }

  def toDependencyTable(individualOutputs:Array[DirectedGraph[String]]): Array[Array[Dependency]] = {
    val length = individualOutputs.head.size + 1 // one more entry for explicit root
    val depMap = toDependencyMap(individualOutputs)

    // stores all candidate dependencies in a table format (from start to end) for easier access
    val deps = new Array[Array[Dependency]](length)
    for(i <- deps.indices) {
      deps(i) = new Array[Dependency](length)
    }

    for(depKey <- depMap.keySet) {
      val votes = depMap.get(depKey)
      assert(votes.nonEmpty)
      val dep = Dependency(depKey._1, depKey._2, depKey._3, votes.get.toSet)
      if(deps(dep.head)(dep.modifier) == null)
        deps(dep.head)(dep.modifier) = dep
      else if(deps(dep.head)(dep.modifier).score < dep.score)
        deps(dep.head)(dep.modifier) = dep
    }

    deps
  }

  def toDirectedGraph(deps:List[Dependency]):DirectedGraph[String] = {
    val edges = new ListBuffer[Edge[String]]
    val roots = new mutable.HashSet[Int]()

    for(dep <- deps) {
      if(dep.head == 0) {
        assert(dep.modifier > 0)
        roots += dep.modifier - 1
      } else {
        assert(dep.modifier > 0)
        assert(dep.head > 0)
        edges += Edge[String](dep.head - 1, dep.modifier - 1, dep.label)
      }
    }

    new DirectedGraph[String](edges.toList, roots.toSet)
  }
}

/**
  * Unlike our representation in DirectedGraph, offsets in this class start at 1; root is 0 in this notation (Eisner requires this)
  */
case class Dependency(head:Int, modifier:Int, label:String, votes:Set[Int]) {
  lazy val score:Double = {
    var s = 0.0
    for(modelIndex <- votes) {
      s += 1.0 - (0.01 * modelIndex)
    }
    s
  }

  override def toString: String = s"($head, $modifier, $label, ${votes.size})"
}

class Span(val deps:List[Dependency], val score:Double) {
  def this() = this(null, 0.0)

  def this(left:Span, right:Span, dep:Dependency) = this(
    Span.addDependencies(left, right, dep),
    left.score + right.score + (if (dep != null) dep.score else 0.0)
  )

  override def toString: String = {
    if(deps != null) "[" + deps.mkString(", ") + "]"
    else "[]"
  }
}

object Span {
  private def addDependencies(left:Span, right:Span, dep:Dependency):List[Dependency] = {
    val l = new ListBuffer[Dependency]
    if(dep != null) l += dep
    if(left.deps != null) l ++= left.deps
    if(right.deps != null) l ++= right.deps
    l.toList
  }
}

class Chart(val dimension:Int) {
  val chart:Array[Array[Array[Span]]] = mkChart(dimension)

  def mkChart(dimension:Int): Array[Array[Array[Span]]] = {
    val c = new Array[Array[Array[Span]]](dimension)
    for(i <- c.indices) {
      c(i) = new Array[Array[Span]](dimension)
      for(j <- c(i).indices) {
        c(i)(j) = new Array[Span](2)
        c(i)(j)(0) = new Span()
        c(i)(j)(1) = new Span()
      }
    }
    c
  }

  def get(start:Int, end:Int, typ:Int): Span = chart(start)(end)(typ)

  def set(start:Int, end:Int, typ:Int, span:Span) {
    if(chart(start)(end)(typ) == null)
      chart(start)(end)(typ) = span
    else if(chart(start)(end)(typ).score < span.score)
      chart(start)(end)(typ) = span
  }
}

object Chart {
  val HEAD_LEFT:Int = 0
  val HEAD_RIGHT:Int = 1
}
