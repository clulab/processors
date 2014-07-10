package edu.arizona.sista.struct

/**
 * Stores a tree structure; used generally to store constituent trees
 * User: mihais
 * Date: 4/1/13
 */
class Tree[L](
               /** A syntactic label, POS tag, or word for terminal nodes */
               val value:L,
               /** Array of children of this node */
               val children:Option[Array[Tree[L]]],
               /** Offset of child that is head for this constituent; starts at 0 */
               val head:Int,
               /** Start token offset in the sentence for this constituent; starts at 0 */
               val startOffset:Int,
               /** Offset of token immediately after this constituent; starts at 0 */
               val endOffset:Int) extends Serializable {

  /** Offset of head token; starts at 0 */
  val headOffset = findHeadPosition

  def isLeaf = (children == None)
  def isPreTerminal = (isUnary && children.get(0).isLeaf)
  def isUnary = (children != None && children.get.length == 1)

  override def toString:String = {
    val os = new StringBuilder
    os.append (this.toStringDepth(0))
    os.toString()
  }

  /** Finds the offset of the head token in the sentence; starts at 0 */
  private def findHeadPosition:Int = {
    if(isLeaf) return startOffset
    children.get(head).findHeadPosition
  }

  def toStringDepth(depth:Int):String = {
    val os = new StringBuilder

    if (isLeaf) {
      os.append (" ")
      os.append(value)
    } else {
      os.append("\n")
      for (i <- 0 until depth) { os.append("    ") }
      os.append("(")
      os.append(value)
      // if(! isPreTerminal) os.append(" " + head)
      for (c <- children.get) {
        os.append(c.toStringDepth(depth+1))
      }
      if (!isPreTerminal) {
        os.append("\n")
        for (i <- 0 until depth) { os.append("    ") }
      }
      os.append(")")
    }
    os.toString()
  }
}
