package org.clulab.processors.hexatagging

trait BHT {
  def isTerminal(): Boolean

  def rightMostLeft:Int
  def leftMostRight:Int

  protected def addOffset(offset: Int): String = {
    val sb = new StringBuilder()
    for(i <- 0 until offset) sb.append(' ')
    sb.toString()
  }

  override def toString(): String = toString(0) 

  def toString(offset: Int): String
}

class TerminalBHT(node: Int, label: String) extends BHT {
  override def isTerminal(): Boolean = true
  override def toString(offset: Int): String = s"${addOffset(offset)}($node, $label)\n"

  override def leftMostRight:Int = node
  override def rightMostLeft: Int = node
}

class NonTerminalBHT(label: String, left: BHT, right: BHT) extends BHT {

  override def rightMostLeft: Int = left.leftMostRight

  override def leftMostRight: Int = right.rightMostLeft

  override def isTerminal(): Boolean = false
  override def toString(offset: Int): String = {
    val sb = new StringBuilder()
    sb.append(s"${addOffset(offset)}$label ($rightMostLeft, $leftMostRight)\n")
    sb.append(left.toString(offset + 2))
    sb.append(right.toString(offset + 2))
    sb.toString()
  }
}