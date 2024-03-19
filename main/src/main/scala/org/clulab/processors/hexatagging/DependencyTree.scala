package org.clulab.processors.hexatagging

import org.clulab.sequences.Row

class DependencyTree(
  val node:Int, 
  val leftMods: Option[Seq[DependencyTree]], 
  val rightMods: Option[Seq[DependencyTree]]) {
  
  def toString(): String = toString(0)

  def toString(offset: Int): String = {
    val sb = new StringBuilder()
    addOffset(sb, offset)
    sb.append(node)
    sb.append("\n")
    addOffset(sb, offset)
    sb.toString()
  }

  private def addOffset(sb: StringBuilder, offset: Int): Unit = {
    for(i <- 0 until offset) sb.append(' ')
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
    for(i <- sent.indices) {

    }
  }
}