package org.clulab.processors.hexatagging

trait BHT {
  def isTerminal(): Boolean

  def firstPosition:Int // inclusive
  def lastPosition:Int // inclusive

  var parent:Option[BHT] = None

  protected def addOffset(offset: Int): String = {
    val sb = new StringBuilder()
    for(i <- 0 until offset) sb.append(' ')
    sb.toString()
  }

  override def toString(): String = toString(0) 

  def toString(offset: Int): String

  def setHexaTags(termTags: Array[String], nonTermTags: Array[String])
}

class TerminalBHT(val node: Int, val label: String) extends BHT {
  override def isTerminal(): Boolean = true
  override def toString(offset: Int): String = s"${addOffset(offset)}($node, $label)\n"

  override def firstPosition: Int = node
  override def lastPosition: Int = node

  override def setHexaTags(termTags: Array[String], nonTermTags: Array[String]): Unit = {
    assert(parent.isDefined)
    assert(! parent.get.isTerminal())

    val hexaTag = 
      if(parent.get.asInstanceOf[NonTerminalBHT].left == this) {
        HexaTags.TERMINAL_LEFT_CHILD + "-" + label
      } else {
        HexaTags.TERMINAL_RIGHT_CHILD + "-" + label
      }

    assert(termTags(node) == null)
    termTags(node) = hexaTag
  }
}

class NonTerminalBHT(val label: String, var left: BHT, var right: BHT) extends BHT {

  override def firstPosition: Int = left.firstPosition
  override def lastPosition: Int = right.lastPosition

  override def isTerminal(): Boolean = false
  override def toString(offset: Int): String = {
    val sb = new StringBuilder()
    sb.append(s"${addOffset(offset)}$label ($firstPosition, $lastPosition)\n")
    sb.append(left.toString(offset + 2))
    sb.append(right.toString(offset + 2))
    sb.toString()
  }

  override def setHexaTags(termTags: Array[String], nonTermTags: Array[String]): Unit = {
    val hexaTag = 
      if(parent.isEmpty) {
        HexaTags.NONTERM_LEFT_CHILD + "-" + label
      } else {
        assert(! parent.get.isTerminal())
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
}