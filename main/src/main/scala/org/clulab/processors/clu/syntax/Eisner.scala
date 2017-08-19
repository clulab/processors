package org.clulab.processors.clu.syntax

import org.clulab.struct.{DirectedGraph, Edge}

import scala.collection.mutable
import Chart._

import scala.collection.mutable.ListBuffer

/**
  * Implements the Eisner dynamic-programmic parsing algorithm, which we use for the ensemble model
  * User: mihais
  * Date: 8/9/17
  */
class Eisner(val individualOutputs:Array[DirectedGraph[String]]) {

  def parse(): DirectedGraph[String] = {
    val candTable = toDependencies(individualOutputs)
    val length = candTable.length
    val chart = new Chart(length)

    for(spanLength <- 2 to length) {
      var start = 0
      while(start + spanLength <= length) {
        val end = start + spanLength - 1
        for(split <- start until end) {

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
        start += 1
      }
    }

    val top = chart.get(0, length - 1, HEAD_LEFT)
    toDirectedGraph(top.deps)
  }

  def toDependencies(individualOutputs:Array[DirectedGraph[String]]): Array[Array[Dependency]] = {
    val length = individualOutputs.head.size + 1 // one more entry for explicit root

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

      // add dependency to root
      val roots = individualOutputs(model).roots
      assert(roots.nonEmpty) // TODO: handle the case of trees wo/ roots here
      // we only consider 1 root, because we must have trees here
      val votes = depMap.getOrElseUpdate(Tuple3(0, roots.toList.min, "root"), new mutable.HashSet[Int]())
      votes += model
    }

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
        Edge[String](dep.head - 1, dep.modifier - 1, dep.label)
      }
    }

    new DirectedGraph[String](edges.toList, roots.toSet)
  }
}

/**
  * Unlike our representation in DirectedGraph, offsets in this class start at 1; root is 0 in this notation
  */
case class Dependency(head:Int, modifier:Int, label:String, votes:Set[Int]) {
  def score:Double = votes.size.toDouble
}

class Span(val deps:List[Dependency], val score:Double) {
  def this() = this(null, 0.0)

  def this(left:Span, right:Span, dep:Dependency) = this(
    Span.addDependencies(left, right, dep),
    left.score + right.score + (if (dep != null) dep.score else 0.0)
  )
}

object Span {
  private def addDependencies(left:Span, right:Span, dep:Dependency):List[Dependency] = {
    val l = new ListBuffer[Dependency]
    if(dep != null) l += dep
    l ++= left.deps
    l ++= right.deps
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
