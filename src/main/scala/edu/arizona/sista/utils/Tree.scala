package edu.arizona.sista.utils

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
  val endOffset:Int) {


  def isLeaf = (children == None)
  def isPreTerminal = (isUnary && children.get(0).isLeaf)
  def isUnary = (children != None && children.get.length == 1)

  override def toString:String = {
    val os = new StringBuilder
    if (isLeaf) {
      os.append(value)
    } else {
      os.append("(")
      os.append(value)
      for (c <- children.get) {
        os.append(" ")
        os.append(c.toString)
      }
      os.append(")")
    }
    os.toString()
  }
}
