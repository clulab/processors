package org.clulab.processors.clu.syntax

import org.clulab.struct.{DirectedGraph, Edge}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Implements the word-by-word voting scheme from Surdeanu et al. (2010)
  * User: mihais
  * Date: 8/9/17
  */
class EnsembleModel(val individualOutputs:Array[DirectedGraph[String]]) {

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

