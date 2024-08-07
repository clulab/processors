package org.clulab.processors.hexatagging

import org.clulab.struct.Edge
import scala.collection.mutable.ListBuffer

trait BHT {
  def isTerminal:Boolean

  def firstPosition:Int // inclusive
  def lastPosition:Int // inclusive
  
  def headPosition:Int
  def dependencyLabel:String

  var parent:Option[BHT] = None

  protected def addOffset(offset: Int): String = " " * offset

  override def toString(): String = toString(0) 

  def toString(offset: Int): String

  def setHexaTags(termTags: Array[String], nonTermTags: Array[String]): Unit

  def toDependencies(deps: ListBuffer[Edge[String]]): Unit
}

class TerminalBHT(val node: Int, val label: String) extends BHT {
  override def isTerminal: Boolean = true
  override def toString(offset: Int): String = s"${addOffset(offset)}($node, $label)\n"

  override def firstPosition: Int = node
  override def lastPosition: Int = node

  override def headPosition: Int = node
  override def dependencyLabel: String = label

  override def setHexaTags(termTags: Array[String], nonTermTags: Array[String]): Unit = {
    assert(parent.isDefined)
    assert(! parent.get.isTerminal)

    val hexaTag = 
      if(parent.get.asInstanceOf[NonTerminalBHT].left == this) {
        HexaTags.TERMINAL_LEFT_CHILD + "-" + label
      } else {
        HexaTags.TERMINAL_RIGHT_CHILD + "-" + label
      }

    assert(termTags(node) == null)
    termTags(node) = hexaTag
  }

  override def toDependencies(deps: ListBuffer[Edge[String]]): Unit = {
    // nothing to do for terminals
  }
}

class NonTerminalBHT(val label: String, var left: BHT, var right: BHT) extends BHT {

  override def firstPosition: Int = 
    if(left != null) left.firstPosition
    else -1
    
  override def lastPosition: Int = 
    if(right != null) right.lastPosition
    else -1

  override def headPosition: Int = {
    if(label == "L") {
      if(left != null) left.headPosition
      else -1
    } else if(label == "R") {
      if(right != null) right.headPosition
      else -1
    } else {
      throw new RuntimeException(s"ERROR: unexpected non-terminal label: $label!")
    }
  }

  override def dependencyLabel: String = {
    if(label == "L") {
      if(left != null) left.dependencyLabel
      else "dummy"
    } else if(label == "R") {
      if(right != null) right.dependencyLabel
      else "dummy"
    } else {
      throw new RuntimeException(s"ERROR: unexpected non-terminal label: $label!")
    }
  }

  override def isTerminal: Boolean = false
  override def toString(offset: Int): String = {
    val sb = new StringBuilder()
    sb.append(s"${addOffset(offset)}$label ($firstPosition, $lastPosition) ($headPosition, $dependencyLabel)\n")
    if(left != null) {
      sb.append(left.toString(offset + 2))
    } else {
      sb.append(addOffset(offset + 2))
      sb.append("dummy\n")
    }
    if(right != null) {
      sb.append(right.toString(offset + 2))
    } else {
      sb.append(addOffset(offset + 2))
      sb.append("dummy\n")
    }
    sb.toString()
  }

  override def setHexaTags(termTags: Array[String], nonTermTags: Array[String]): Unit = {
    val hexaTag = 
      if(parent.isEmpty) {
        HexaTags.NONTERM_LEFT_CHILD + "-" + label
      } else {
        assert(! parent.get.isTerminal)
        if(parent.get.asInstanceOf[NonTerminalBHT].left == this) {
          HexaTags.NONTERM_LEFT_CHILD + "-" + label
        } else {
          HexaTags.NONTERM_RIGHT_CHILD + "-" + label
        }
      }

    val offset = left.lastPosition
    if(nonTermTags(offset) != null) {
      throw new RuntimeException(s"Found non empty nonterm tag: ${nonTermTags(offset)} at position $offset!")
    }
    nonTermTags(offset) = hexaTag
    // println(s"NONTERM HEXATAGS: ${nonTermTags.mkString(" ")}")
    left.setHexaTags(termTags, nonTermTags)
    right.setHexaTags(termTags, nonTermTags)
  }

  /**
    * Produces the dependency tree corresponding to this BHT
    *
    * @return The dependency tree represented as 
    *   a list of edges (source = linguistic head, destination = modifier) and 
    *   a set of root nodes
    */
  override def toDependencies(deps: ListBuffer[Edge[String]]): Unit = {
    if(isInstanceOf[NonTerminalBHT] && left != null && right != null) {
      // head is the left child
      if(label == "L") {
        val dep = new Edge[String](left.headPosition, right.headPosition, right.dependencyLabel)
        deps += dep
      } 
      // head is the right child
      else if(label == "R") {
        val dep = new Edge[String](right.headPosition, left.headPosition, left.dependencyLabel)
        deps += dep
      }

      left.toDependencies(deps)
      right.toDependencies(deps)
    }
  }
}