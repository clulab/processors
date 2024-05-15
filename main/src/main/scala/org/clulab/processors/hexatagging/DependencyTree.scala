package org.clulab.processors.hexatagging

import org.clulab.sequences.Row
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

class DependencyTree(
  val node:Int, 
  val leftMods: ArrayBuffer[DependencyTree] = new ArrayBuffer[DependencyTree], 
  val leftLabels: ArrayBuffer[String] = new ArrayBuffer[String],
  val rightMods: ArrayBuffer[DependencyTree] = new ArrayBuffer[DependencyTree],
  val rightLabels: ArrayBuffer[String] = new ArrayBuffer[String]) {
  
  override def toString(): String = toString(0)

  def toString(offset: Int): String = {
    val sb = new StringBuilder()
    addOffset(sb, offset)
    sb.append(node)
    sb.append("\n")

    if(leftMods.nonEmpty) {
      addOffset(sb, offset)
      sb.append("Left:")
      sb.append("\n")
      for(c <- leftMods) {
        sb.append(c.toString(offset + 2))
      }
    }

    if(rightMods.nonEmpty) {
      addOffset(sb, offset)
      sb.append("Right:")
      sb.append("\n")
      for(c <- rightMods) {
        sb.append(c.toString(offset + 2))
      }
    }

    sb.toString()
  }

  def addLeft(t: DependencyTree, l: String): Unit = {
    leftMods += t
    leftLabels += l
  }

  def addRight(t: DependencyTree, l: String): Unit = {
    rightMods += t
    rightLabels += l
  }

  private def addOffset(sb: StringBuilder, offset: Int): Unit = {
    for(i <- 0 until offset) sb.append(' ')
  }

  def toBHT(stack: Stack[BHT], label: String): Unit = {
    stack.push(new TerminalBHT(node, label))

    for(i <- leftMods.indices) {
      leftMods(i).toBHT(stack, leftLabels(i))
      val left = stack.pop()
      val right = stack.pop()
      val nonTerm = new NonTerminalBHT("R", left, right)
      left.parent = Some(nonTerm)
      right.parent = Some(nonTerm)
      stack.push(nonTerm)
    }

    for(i <- rightMods.indices) {
      rightMods(i).toBHT(stack, rightLabels(i))
      val right = stack.pop() // there is a bug in Alg 1 in the paper here
      val left = stack.pop()
      val nonTerm = new NonTerminalBHT("L", left, right)
      right.parent = Some(nonTerm)
      left.parent = Some(nonTerm)
      stack.push(nonTerm)
    }
  }
}

object DependencyTree {
  /**
    * Converts one sentence into a DependencyTree
    * Each Row follows our .labels format: word POS _ label head
    *   where the "head" value is the absolute position of the head token (-1 for root)
    * @param sent
    * @return
    */
  def toTree(sent: Array[Row]): DependencyTree = {
    var root: Option[DependencyTree] = None
    var subtrees = new HashMap[Int, DependencyTree]()

    for(i <- sent.indices) {
      val head = sent(i).get(4).toInt
      val label = sent(i).get(3)
      val modTree = subtrees.getOrElseUpdate(i, new DependencyTree(i))

      if(head == -1) {
        // found the root node
        root = Some(modTree)
      } else {
        val headTree = subtrees.getOrElseUpdate(head, new DependencyTree(head))
        if(i < head) headTree.addLeft(modTree, label)
        else headTree.addRight(modTree, label)
      }
    }

    assert(root.isDefined)
    root.get
  }
}