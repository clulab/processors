package org.clulab.struct

import org.clulab.processors.Processor

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * A more efficient trie implementation, where the first layer is stored as a hash map for efficiency (the rest are the usual trees)
  * The find* methods are thread safe
  * User: mihais
  * Date: 5/12/15
  */
@SerialVersionUID(1000L)
class IntHashTrie(val caseInsensitive: Boolean = true, val internStrings: Boolean = true) extends Serializable {
  /** Stores the first layer, i.e., the entry points in the trie */
  val entries = new mutable.HashMap[String, IntTrieNode]()

  def toString(stringBuilder: StringBuilder, labels: Seq[String]): Unit = {
    entries.values.foreach { trieNode =>
      trieNode.toString(stringBuilder, Some(labels))
      stringBuilder.append("\n")
    }
  }

  def entriesSize: Int = entries.size

  protected def in(s: String):String = {
    val casedS = if (caseInsensitive) s.toLowerCase else s
    val internedS = if (internStrings) Processor.internString(casedS) else casedS

    internedS
  }

  def add(tokens: Array[String], completePath: Int): Unit = {
    if (tokens != null && tokens.length > 0) {
      // first layer
      val inTokens = tokens.map(in) // Do them all at once so that trie doesn't need to
      val token = inTokens.head
      val tree = entries.get(token).map { tree =>
        // If there was already a completePath, do not overwrite.it.
        if (tokens.length == 1 && tree.completePath < 0) tree.completePath = completePath
        tree
      }
      .getOrElse {
        val tree = new IntTrieNode(token, if (tokens.length == 1) completePath else -1)

        entries.put(token, tree)
        tree
      }
      tree.add(inTokens, 1, completePath)
    }
  }

  /**
    * Returns the length of the matched span, which may be 0,
    * along with the value of completePath for that match.
    * When multiple paths are found, the longest one is kept
    * Text must be normalized (i.e., case folding) BEFORE this call, if necessary!
    */
  def findAt(sequenceNormalized: Array[String], offset: Int): IntTrieNode.Match = {
    val longestMatch = new IntTrieNode.Match()

    entries.get(sequenceNormalized(offset)).map { tree =>
      tree.find(sequenceNormalized, offset, 0, longestMatch)
    }
    longestMatch
  }
}

object IntTrieNode {
  class Match(var length: Int = 0, var completePath: Int = -1)
}

@SerialVersionUID(1000L)
// CompletePath is negative if the path is not complete.  If it is >= 0, then it is complete
// and the number is eventually  used as an index into labels.
case class IntTrieNode(token:String, var completePath: Int, var children: Option[ListBuffer[IntTrieNode]]) extends Serializable {

  def this(token: String, completePath: Int) = this(token, completePath, None)

  def toString(stringBuilder: StringBuilder, labels: Option[Seq[String]]): Unit = {
    stringBuilder.append(token)
    if (completePath >= 0) {
      stringBuilder.append("*")
      if (labels.isDefined)
        stringBuilder.append(labels.get(completePath))
    }
    children.foreach { children: ListBuffer[IntTrieNode] =>
      stringBuilder.append(" (")
      children.zipWithIndex.foreach { case (child: IntTrieNode, index: Int) =>
        if (index > 0)
          stringBuilder.append(" | ")
        child.toString(stringBuilder, labels)
      }
      stringBuilder.append(")")
    }
  }

  override def toString: String = {
    val stringBuilder = new StringBuilder
    toString(stringBuilder, None)
    stringBuilder.toString()
  }

  /**
    * @param sequence          Text to match against
    * @param startOffset            Start token in the sequence
    * @param currentSpanLength How many tokens have we matched so far
    * @param longestMatch      The value of the longest match interval
    * @return true if search should stop here; false otherwise
    */
  def find(sequence: Array[String],
      startOffset: Int,
      currentSpanLength: Int,
      longestMatch: IntTrieNode.Match): Boolean = {
    val currentOffset = startOffset + currentSpanLength

    if (currentOffset >= sequence.length)
      true
    else {
      val comp = token.compareTo(sequence(currentOffset))

      if (comp < 0)
        false // The token is smaller, so don't stop but continue search for larger tokens.
      else if (comp > 0)
        true // The token is already larger, so stop searching through tokens.
      else {
        if (completePath >= 0 && currentSpanLength + 1 > longestMatch.length) {
          longestMatch.length = currentSpanLength + 1
          longestMatch.completePath = completePath
        }
        if (children.isDefined)
          children.get.find(_.find(sequence, startOffset, currentSpanLength + 1, longestMatch))
        true
      }
    }
  }

  def add(tokens: Array[String], offset: Int, completePath: Int): Unit = {
    if (offset < tokens.length) {
      // Don't necessarily need a new one if it is found in the tree already
      val child = addTokenToTree(tokens(offset), completePath, offset == tokens.length - 1)

      child.add(tokens, offset + 1, completePath)
    }
  }

  private def addTokenToTree(newToken: String, completePath: Int, newCompletePath: Boolean): IntTrieNode = {
    if (children.isEmpty)
      children = Some(new ListBuffer[IntTrieNode])

    val someChildren = children.get
    val (index, child) = {
      // Attempt not to go through list twice by leaking matching child.
      var foundChild: Option[IntTrieNode] = None
      val index = someChildren.indexWhere { child =>
        val comp = newToken.compareTo(child.token)
        val found = comp <= 0

        if (comp == 0)
          foundChild = Some(child)
        found
      }
      (index, foundChild)
    }

    if (child.isDefined) {
      val someChild = child.get

      // This node already exists; just adjust the complete path flag, if necessary.
      if (newCompletePath && someChild.completePath < 0)
        someChild.completePath = completePath
      someChild
    }
    else {
      val newChild = new IntTrieNode(newToken, if (newCompletePath) completePath else -1)

      if (index >= 0)
        someChildren.insert(index, newChild)
      else
        // The new child is lexicographically "higher" than all existing children.
        someChildren += newChild
      newChild
    }
  }
}
