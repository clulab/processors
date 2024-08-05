package org.clulab.processors.hexatagging

import org.clulab.struct.{Edge, MinHeap, HeapElement}

import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import java.io.PrintStream

class HexaDecoder {
  /**
    * Produces the best dependency tree given the provided hexa tags
    *
    * @param termTags Array of terminal hexa tags
    * @param nonTermTags Array of non-terminal hexa tags
    * @return The dependency tree represented as 
    *   a list of edges (source = linguistic head, destination = modifier) and 
    *   a set of root nodes
    */
  def decode(
    termTags: Array[Array[(String, Float)]], 
    nonTermTags: Array[Array[(String, Float)]],
    topK: Int, 
    verbose: Boolean = false
  ): (Option[BHT], Option[List[Edge[String]]], Option[Set[Int]]) = {

    // terms must start with TERMINAL_LEFT_CHILD
    termTags(0) = termTags(0).filter(_._1.startsWith(HexaTags.TERMINAL_LEFT_CHILD))
    // non-terms must start with NONTERM_LEFT_CHILD
    nonTermTags(0) = nonTermTags(0).filter(_._1.startsWith(HexaTags.NONTERM_LEFT_CHILD))

    val termPaths = TopKPaths.findTopK(termTags, topK)
    val nonTermPaths = TopKPaths.findTopK(nonTermTags, topK)
    val pathPairs = mergePaths(termPaths, nonTermPaths, topK)

    var bestBht: Option[BHT] = None
    var bestDeps: Option[List[Edge[String]]] = None
    var bestRoots: Option[Set[Int]] = None
    var successIteration = -1

    for(i <- pathPairs.indices if bestBht.isEmpty) {
      val termSeq = pathPairs(i).termPath.sequence
      val nonTermSeq = pathPairs(i).nonTermPath.sequence
      try {
        @annotation.nowarn("cat=deprecation")
        val stack = new Stack[BHT]
        decodeInternal(stack, termSeq, nonTermSeq, verbose)
        val bht = stack.pop()
        val deps = new ListBuffer[Edge[String]]
        bht.toDependencies(deps)
        val roots = findRoots(deps.toSeq, termTags.length)

        // success!
        bestBht = Some(bht)
        bestDeps = Some(deps.toList)
        bestRoots = Some(roots) 
        successIteration = i
      } catch {
        case e: Throwable => // try again with a different set of paths
          // System.err.println(s"Failed to decode the following sentence with beam $i:")
          // printTags(System.err, termSeq, nonTermSeq)
      }
    }

    // if(successIteration > 0) println(s"success iteration = $successIteration")
    (bestBht, bestDeps, bestRoots)
  }

  class PathPair(
    val score: Float, 
    val termPath: Path[String], 
    val nonTermPath: Path[String]) extends HeapElement

  private def mergePaths(
    termPaths: Seq[Path[String]], // must be sorted in descending order of scores
    nonTermPaths: Seq[Path[String]], // must be sorted in descending order of scores
    topK: Int): Seq[PathPair] = {
    val pairHeap = new MinHeap(topK)  
    for(i <- termPaths.indices) {
      var stillWorking = true
      for(j <- nonTermPaths.indices if stillWorking) {
        val score = termPaths(i).score + nonTermPaths(j).score
        val pair = new PathPair(score, termPaths(i), nonTermPaths(j))
        // if insert fails it means the new pair's score < the min score already in the heap
        // since columns are sorted in descending order of scores, we can stop here
        stillWorking = pairHeap.insert(pair)
      }
    }
    pairHeap.toSortedSeq.map(_.asInstanceOf[PathPair])
  }

  def findRoots(deps: Seq[Edge[String]], sentLen:Int): Set[Int] = {
    val mods = deps.map(_.destination).toSet
    val roots = new HashSet[Int]
    for(i <- 0 until sentLen) {
      if(! mods.contains(i)) {
        roots += i
      }
    }
    roots.toSet
  }

  private def printTags(
    pw: PrintStream,
    termTags: Seq[String], 
    nonTermTags: Seq[String]
  ): Unit = {
    pw.println(s"Terminal tags: ${termTags.mkString(", ")}")
    pw.println(s"Nonterm tags: ${nonTermTags.mkString(", ")}")
  }

  private def decodeInternal(
    @annotation.nowarn("cat=deprecation")
    stack: Stack[BHT], 
    termTags: Seq[String], 
    nonTermTags: Seq[String], 
    verbose: Boolean
  ): Unit = {
    // these should always be true
    assert(termTags.length > 1) // this decoder assumes at least 2 words in the sentence
    assert(termTags.length == nonTermTags.length)

    if(verbose) {
      printTags(System.out, termTags, nonTermTags)
    }

    for(i <- termTags.indices) {
      //
      // 1. first process the current terminal tag
      //
      val termTag = termTags(i) 
      if(termTag.startsWith(HexaTags.TERMINAL_RIGHT_CHILD) && stack.isEmpty) {
        throw new RuntimeException(s"ERROR: expected a ${HexaTags.TERMINAL_LEFT_CHILD}!")
      }
      if(verbose) println(s"Processing terminal tag: $termTag")
      
      if(termTag.startsWith(HexaTags.TERMINAL_LEFT_CHILD)) {
        // shift the leaf node into the stack
        val bht = new TerminalBHT(i, termTag.substring(HexaTags.TERMINAL_LEFT_CHILD.length + 1))
        if(verbose) println(s"Pushing:\n${bht}onto the stack.")
        stack.push(bht)
      } else if(termTag.startsWith(HexaTags.TERMINAL_RIGHT_CHILD)) {
        // pop the subtree on the top of the stack,
        // replace the dummy node in the subtree with the terminal node, and
        // push the subtree back to the stack
        val top = stack.pop()
        // this should never happen because we alternate between processing terms and non-terms
        assert(! top.isTerminal)
        val term = new TerminalBHT(i, termTag.substring(HexaTags.TERMINAL_RIGHT_CHILD.length + 1))
        val parentOfDummy = findParentWithDummyChild(top)
        if(parentOfDummy != null) {
          parentOfDummy.right = term
        } else {
          throw new RuntimeException(s"ERROR: expecting a nonTerm with one dummy child!")
        }
        if(verbose) println(s"Pushing:\n${top}onto the stack.")
        stack.push(top)
      } else {
        throw new RuntimeException(s"ERROR: unknown terminal tag $termTag!")
      }

      //
      // 2. then process the next non-terminal tag, unless we reached eos
      //
      if(i < termTags.length - 1) {
        // we must have at least one element on the stack at this stage (see below)
        if(stack.isEmpty) {
          throw new RuntimeException("ERROR: cannot have an empty stack when processing non-terminal tags!")
        }

        val nonTermTag = nonTermTags(i)
        // if the stack has only 1 element, we must process a NONTERM_LEFT_CHILD
        if(nonTermTag.startsWith(HexaTags.NONTERM_RIGHT_CHILD) && stack.length == 1) {
          throw new RuntimeException(s"ERROR: expected a ${HexaTags.NONTERM_LEFT_CHILD}!")
        }
        if(verbose) println(s"Processing non-terminal tag: $nonTermTag")

        if(nonTermTag.startsWith(HexaTags.NONTERM_LEFT_CHILD)) {
          // create a new node labeled R (respectively, L),
          // attach the top element in the stack as its left child, and 
          // attach a dummy node as its right child
          // push it on the stack

          val top = stack.pop()
          val label = nonTermTag.substring(HexaTags.NONTERM_LEFT_CHILD.length + 1)
          val bht = new NonTerminalBHT(label, top, null)
          if(verbose) println(s"Pushing:\n${bht}onto the stack.")
          stack.push(bht)
        } else if(nonTermTag.startsWith(HexaTags.NONTERM_RIGHT_CHILD)) {
          // pop the top element of the stack, 
          // attach it as the new node’s left child, and 
          // set a dummy node as the node’s right child;
          // pop another subtree of the stack, 
          // identify the dummy node in the subtree, and 
          // replace it with the newly created subtree; 
          // push the subtree back to the stack

          val top1 = stack.pop()
          val label = nonTermTag.substring(HexaTags.NONTERM_RIGHT_CHILD.length + 1)
          val bht = new NonTerminalBHT(label, top1, null)
          val top2 = stack.pop()
          val parentOfDummy = findParentWithDummyChild(top2)
          if(parentOfDummy != null) {
            parentOfDummy.right = bht // TODO: I think the dummy node is always on the right?
          } else {
            throw new RuntimeException("ERROR: expected a non-terminal BHT with a dummy child!")
          }
          stack.push(top2)
          if(verbose) println(s"Pushing:\n${top2}onto the stack.")
        } else {
          // this should never happen
          throw new RuntimeException(s"ERROR: unknown non-terminal tag $nonTermTag!")
        }
      }
    }

    if(stack.size != 1){
      // this means the top element on the stack is incomplete, i.e., it covers only part of the sentence
      throw new RuntimeException("ERROR: must have exactly one element in the stack at the end!")
    }
  }

  private def findParentWithDummyChild(bht: BHT): NonTerminalBHT = {
    if(bht == null || bht.isInstanceOf[TerminalBHT]) {
      return null
    } else {
      val nonTerm = bht.asInstanceOf[NonTerminalBHT]
      if(nonTerm.right == null) {
        return nonTerm // TODO: I think the dummy must be on the right
      } else {
        val left = findParentWithDummyChild(nonTerm.left)
        if(left != null) return left
        val right = findParentWithDummyChild(nonTerm.right)
        if(right != null) return right
        return null
      }
    }
  }
}
