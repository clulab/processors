package org.clulab.processors.clu.syntax

import org.clulab.struct.{Counter, DirectedGraph, Edge}

import scala.collection.breakOut
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Implements the word-by-word voting scheme from Surdeanu et al. (2010)
  * User: mihais
  * Date: 8/9/17
  * Last Modified: Let Scala efficiently determine conversion return types.
  */
class EnsembleModel(val individualOutputs:Array[DirectedGraph[String]]) {

  def parse(): DirectedGraph[String] = parseAttardi()

  /**
    * Produces an ensemble parse that is guaranteed to not have cycles (using the Attardi algorithm)
    * Based on the paper: "Reverse Revision and Linear Tree Combination for Dependency Parsing" by Attardi and Dell'Orletta
    * @return the DirectedGraph corresponding to the ensemble parse
    */
  def parseAttardi(): DirectedGraph[String] = {
    val individualDeps = toDependencyList(individualOutputs)
    val sentenceSize = individualOutputs(0).size

    var bestDeps:List[Dependency] = null
    var bestRootCount:Int = Int.MaxValue
    var bestMinVotes = 0
    val rootVotes = 1
    //for (rootVotes <- individualOutputs.length to 1 by -1) {
      val treeDeps = topDownEnsemble(individualDeps, rootVotes)
      val rootCount = countRoots(treeDeps, sentenceSize)

      if(rootCount == 1 || rootVotes == 1) { // rootCount < bestRootCount) {
        bestDeps = treeDeps
        bestRootCount = rootCount
        bestMinVotes = rootVotes
      }
    //}

    //println(s"Chose bestRootCount = $bestRootCount for bestMinVotes = $bestMinVotes")
    EnsembleModel.rootCounts.incrementCount(bestRootCount)

    assert(bestDeps != null)
    toDirectedGraph(bestDeps)
  }


  def countRoots(dependencies: List[Dependency], sentenceSize: Int):Int = {
    var rootCount = 0
    for(dep <- dependencies) {
      if(dep.head == 0) {
        rootCount += 1
      }
    }
    rootCount
  }

  def topDownEnsemble(individualDeps:List[Dependency], minRootVotes:Int): List[Dependency] = {
    var deps = individualDeps

    // stores the ensembled tree wo/ cycles (T)
    val treeDeps = new ListBuffer[Dependency]()
    // stores the nodes that have been traversed
    // initialized with root (0) because this is a top-down traversal
    val treeNodes = new mutable.HashSet[Int]()
    treeNodes += 0 // root

    // counts how many modifiers each head node has
    val modifierCounts = new Counter[Int]()
    for(dep <- deps) modifierCounts.incrementCount(dep.head)

    // the fringe, i.e., the set of arcs whose parent is in T and that can be added to T without affecting the invariant.
    var F = new ListBuffer[Dependency]

    // find all actual roots, i.e., nodes that are headed by 0
    val rootlessDeps = new ListBuffer[Dependency]
    for(dep <- deps) {
      if(dep.head == 0 && dep.votes.size >= minRootVotes) {
        F += dep
      } else {
        rootlessDeps += dep
      }
    }

    val bestRoot = rootlessDeps.sortBy(0 - _.votes.size).head

    val rdeps = new ListBuffer[Dependency]
    rdeps += bestRoot
    deps = rdeps.toList

    //deps = rootlessDeps.toList
    //println(s"F = $F")

    // top-down traversal of the whole tree
    while(F.nonEmpty) {
      var bestScore = -1.0
      var bestModifierCount = -1
      var bestDep:Dependency = null

      // find the best dependency whose head is in F
      // choose the dep with the highest score
      //   break ties by keeping the dep whose modifier serves as head to most nodes (so we can grow the fringe faster)
      //   (the above heuristic is different from Attardi's algorithm; it helps in practice)
      for(f <- F) {
        if(treeNodes.contains(f.head) &&
           (f.score > bestScore ||
             (f.score == bestScore && modifierCounts.getCount(f.modifier) > bestModifierCount))) {
          bestScore = f.score
          bestModifierCount = modifierCounts.getCount(f.modifier).toInt
          bestDep = f
        }
      }

      // step 1: add the best current dep to T
      assert(bestDep != null)
      treeDeps += bestDep
      treeNodes += bestDep.modifier
      //println(s"bestDep = $bestDep")

      // step 2: remove from F all deps whose modifier is bestDep.modifier
      var newF = new ListBuffer[Dependency]
      for(f <- F) {
        if(! treeNodes.contains(f.modifier)) {
          newF += f
        }
      }
      F = newF

      // step 3: adds to F all arcs (h', d', r') in the original trees where h' \in T and d' not \in T
      var newDeps = new ListBuffer[Dependency]
      for(dep <- deps) {
        if(treeNodes.contains(dep.head) && ! treeNodes.contains(dep.modifier)) {
          F += dep
        } else {
          newDeps += dep
        }
      }
      deps = newDeps.toList
    }

    treeDeps.toList
  }

  /**
    * Produces an ensemble parse using the word-by-word voting scheme from Surdeanu et al. (2010)
    * Note: this works well and is fast, but it does not guarantee that the output is a tree
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
    depMap.map {                           // create and return the actual dependency list
      case (depKey, votes) =>
        Dependency(depKey._1, depKey._2, depKey._3, votes.toSet)
    }(breakOut)
  }

  def toDirectedGraph(deps:List[Dependency]):DirectedGraph[String] = {
    val edges = new ListBuffer[Edge[String]]
    val roots = new mutable.HashSet[Int]()

    deps.foreach { dep =>
      if(dep.head == 0) {
        assert(dep.modifier > 0)
        roots += dep.modifier - 1
      } else {
        assert(dep.modifier > 0)
        assert(dep.head > 0)
        edges += Edge[String](dep.head - 1, dep.modifier - 1, dep.label)
      }
      ()                                    // workaround for bug #10151
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

object EnsembleModel {
  val rootCounts:Counter[Int] = new Counter[Int]()
}

