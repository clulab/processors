package org.clulab.processors.hexatagging

import scala.collection.mutable.Stack
import _root_.org.clulab.struct.Terminal
import org.clulab.struct.NonTerminal
import org.clulab.struct.Edge
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

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
    nonTermTags: Array[Array[(String, Float)]]
  ): (BHT, List[Edge[String]], Set[Int]) = {
    val stack = new Stack[BHT]
    decode(stack, termTags, nonTermTags)
    val bht = stack.pop()
    val deps = new ListBuffer[Edge[String]]
    bht.toDependencies(deps)
    val roots = findRoots(deps, termTags.length)
    (bht, deps.toList, roots)
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

  def decode(
    stack: Stack[BHT], 
    termTags: Array[Array[(String, Float)]], // assumes sorted in descending order of scores
    nonTermTags: Array[Array[(String, Float)]], // assumes sorted in descending order of scores
    verbose: Boolean = false
  ): Unit = {
    assert(termTags.length > 1) // this decoder assumes at least 2 words in the sentence
    assert(termTags.length == nonTermTags.length)
    for(i <- termTags.indices) {
      //
      // 1. first process the current terminal tag
      //
      val termTag: String = 
        // if the stack is empty, we can't use a TERMINAL_RIGHT_CHILD
        if(stack.isEmpty) {
          val tagOpt = termTags(i).find(_._1.startsWith(HexaTags.TERMINAL_LEFT_CHILD))
          if(tagOpt.isEmpty) {
            throw new RuntimeException(s"ERROR: expected a ${HexaTags.TERMINAL_LEFT_CHILD}!")
          }
          tagOpt.get._1
        } else {
          termTags(i).head._1
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

        // if the stack has only 1 element, we must process a NONTERM_LEFT_CHILD
        val nonTermTag = 
          if(stack.length == 1) {
            val tagOpt = nonTermTags(i).find(_._1.startsWith(HexaTags.NONTERM_LEFT_CHILD))
            if(tagOpt.isEmpty) {
              throw new RuntimeException(s"ERROR: expected a ${HexaTags.NONTERM_LEFT_CHILD}!")
            }
            tagOpt.get._1
          } else {
            nonTermTags(i).head._1
          }
        if(verbose) println(s"Processing non-terminal tag: $nonTermTag")

        if(nonTermTag.startsWith(HexaTags.NONTERM_LEFT_CHILD)) {
          // create a new node labeled R (respectively, L),
          // attach the top element in the stack as its left child, and 
          // attach a dummy node as its right child

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
          throw new RuntimeException(s"ERROR: unknown non-terminal tag $nonTermTag!")
        }
      }
    }
    assert(stack.size == 1) // must have 1 element on the stack at the end
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
