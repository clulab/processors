package org.clulab.struct

import scala.util.parsing.combinator._

/**
 * Stores a tree structure; used generally to store constituent trees
 * User: marcov, mihais
 * Date: 4/1/13, modified 10/13/14
 */

trait Tree {
  protected var _parent: Option[NonTerminal] = None
  def parent = _parent

  def setParent(p:Option[NonTerminal]) { _parent = p }

  /** A syntactic label, POS tag, or word for terminal nodes */
  def value:String

  /** Array of children of this node */
  def children:Option[Array[Tree]]

  /** Offset of *immediate* child that is head for this constituent; starts at 0 */
  def head:Int

  /** Start token offset in the sentence for this constituent; starts at 0 */
  def startOffset:Int

  /** Offset of token immediately after this constituent; starts at 0 */
  def endOffset:Int

  /** Offset of head token; starts at 0 */
  def headOffset:Int

  def isLeaf:Boolean

  def isPreTerminal = this match {
    case NonTerminal(label, Array(Terminal(token))) => true
    case _ => false
  }

  def isUnary:Boolean = children != None && children.get.length == 1

  override def toString = this match {
    case Terminal(token) => token
    case NonTerminal(tag, children) => s"($tag ${children.map(_.toString).mkString(" ")})"
  }

  /** Pretty print, including position of head child */
  def toStringDepth(showHead:Boolean, depth:Int = 0):String = {
    val os = new StringBuilder

    if (isLeaf) {
      os.append (" ")
      os.append(value)
    } else {
      os.append("\n")
      for (i <- 0 until depth) { os.append("    ") }
      os.append("(")
      os.append(value)
      if(showHead) if(! isPreTerminal) os.append(" " + head)
      for (c <- children.get) {
        os.append(c.toStringDepth(showHead, depth + 1))
      }
      if (!isPreTerminal) {
        os.append("\n")
        for (i <- 0 until depth) { os.append("    ") }
      }
      os.append(")")
    }

    os.toString()
  }

  def apply(address: Int*): Tree = apply(address.toList)

  def apply(address: List[Int]): Tree = (this, address) match {
    case (n, Nil) => n
    case (n: NonTerminal, i :: addr) => n._children(i).apply(addr)
    case _ => throw new Error("apply error")
  }

  def treePosition = {
    def addressBuilder(node: Tree, addr: List[Int]): List[Int] =
      node.parent match {
        case None => addr
        case Some(p) => addressBuilder(p, p._children.indexOf(node) :: addr)
      }
    addressBuilder(this, Nil)
  }

  def root: Tree = parent match {
    case None => this
    case Some(node) => node.root
  }

  def hasLabel(label: String) = this match {
    case NonTerminal(`label`, _) => true
    case _ => false
  }

  def preterminals = {
    def collect(nodes: List[Tree], preterms: List[NonTerminal]): List[NonTerminal] = nodes match {
      case Nil => preterms
      case (n @ NonTerminal(label, Array(Terminal(token)))) :: rest => collect(rest, n :: preterms)
      case NonTerminal(label, children) :: rest => collect(children.toList ++ rest, preterms)
      case Terminal(token) :: rest => collect(rest, preterms)
      case _ => throw new Error("preterminals error")
    }
    collect(List(this), Nil).reverse
  }

  def terminals = preterminals map (_._children.head.asInstanceOf[Terminal])
  def tokens = terminals map (_.token)
  def posTags = preterminals map (n => (n.label, n._children.head.asInstanceOf[Terminal].token))

  /** returns ancestors sorted from near to far */
  def ancestors: List[NonTerminal] = parent match {
    case None => Nil
    case Some(p) => p :: p.ancestors
  }

  def leftSibling: Option[Tree] = parent flatMap { node =>
    val i = node._children indexOf this
    if (i == 0) None
    else Some(node._children(i - 1))
  }

  def rightSibling: Option[Tree] = parent flatMap { node =>
    val i = node._children indexOf this
    if (i + 1 == node._children.size) None
    else Some(node._children(i + 1))
  }

  private def assignIndicesToTerminals() {
    for ((t, i) <- terminals.zipWithIndex)
      t.setIndex(i)
  }

  private def assignIndicesToNonTerminals() {
    this match {
      case Terminal(token) =>
      case n @ NonTerminal(label, children) =>
        children.map(_.assignIndicesToNonTerminals())
        n.setStartEndIndices(n._children.head.startOffset, n._children.last.endOffset)
    }
  }

  protected def findHeadPosition:Int = {
    if(isLeaf) return startOffset
    children.get(head).findHeadPosition
  }
}

case class Terminal(token: String) extends Tree with Serializable {
  /** this node's index in the terminal list; set after parsing tree in Tree.parse */
  private var _index = -1
  def index = _index

  def value:String = token
  def children:Option[Array[Tree]] = None
  def head:Int = index
  def startOffset:Int = index
  def endOffset:Int = index + 1
  def headOffset:Int = index
  def isLeaf:Boolean = true

  def setIndex(i:Int) { _index = i }
}

case class NonTerminal(label: String, _children: Array[Tree]) extends Tree with Serializable {
  /** this node's start offset in the terminal list */
  private var _startIndex = -1

  /** this node's end offset in the terminal list */
  private var _endIndex = -1

  /** offset of the immediate child that is the head of this non-terminal */
  private var _head = -1

  /** Offset of terminal that is head for this constituent; starts at 0 */
  private var _headIndex = -1

  _children foreach (_.setParent(Some(this)))  // I am your father

  def value:String = label
  def children:Option[Array[Tree]] = Some(_children)
  def head:Int = _head
  def startOffset:Int = _startIndex
  def endOffset:Int = _endIndex
  def headOffset:Int = _headIndex
  def isLeaf:Boolean = false

  def setStartEndIndices(s:Int, e:Int) {
    _startIndex = s
    _endIndex = e
  }

  def setHead(h:Int) {
    _head = h
    if(_head != -1) {
      _headIndex = findHeadPosition
    }
  }
}

object Tree {
  def mkTree(input: String): Tree = {
    val tree = TreeParser.parse(input)
    tree.assignIndicesToTerminals()
    tree.assignIndicesToNonTerminals()
    tree
  }

  def getCommonAncestor(nodes: Tree*): NonTerminal = getCommonAncestor(nodes.toList)

  def getCommonAncestor(nodes: List[Tree]): NonTerminal = {
    val allAncestors = nodes.map(_.ancestors.reverse)
    val minLength = allAncestors.map(_.size).min
    val numDistinct = for (i <- 0 until minLength) yield allAncestors.map(_(i)).distinct.size
    val i = numDistinct.indexWhere(_ > 1)
    if (i > 0) allAncestors(0)(i - 1)  // previous node was common ancestor
    else if (i < 0) allAncestors(0)(minLength - 1)  // return last possible ancestor
    else throw new RuntimeException("ERROR: no common ancestor")
  }

  def getPath(from: Tree, to: Tree, upSep: String = " ^ ", downSep: String = " v ") = {
    val common = getCommonAncestor(from, to)
    val fromPath = from.ancestors takeWhile (_ != common) map (_.label)
    val toPath = to.ancestors takeWhile (_ != common) map (_.label)
    fromPath.mkString(upSep) + upSep + common.label + downSep + toPath.reverse.mkString(downSep)
  }

  private object TreeParser extends RegexParsers {
    def parse(input: String): Tree = parseAll(tree, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => sys.error(failure.msg)
    }
    def tree: Parser[Tree] = terminal | nonTerminal
    def token: Parser[String] = """[^()\s]+""".r
    def terminal: Parser[Terminal] = token ^^ { new Terminal(_) }
    def nonTerminal: Parser[NonTerminal] = "(" ~> token ~ rep1(tree) <~ ")" ^^ {
      case tag ~ children => new NonTerminal(tag, children.toArray)
    }
  }
}

