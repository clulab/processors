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
class BooleanHashTrie(val label: String, val caseInsensitive: Boolean = true, val internStrings: Boolean = true) extends Serializable {
  /** Stores the first layer, i.e., the entry points in the trie */
  protected val entries = new mutable.HashMap[String, BooleanTrieNode]()
  val bLabel: String = "B-" + label
  val iLabel: String = "I-" + label

  def toString(stringBuilder: StringBuilder): Unit = {
    entries.values.foreach { trieNode =>
      trieNode.toString(stringBuilder, Some(label))
      stringBuilder.append("\n")
    }
  }

  override def toString: String = {
    val stringBuilder = new StringBuilder()

    toString(stringBuilder)
    stringBuilder.toString
  }

  def entriesSize: Int = entries.size

  protected def in(s: String): String = {
    val casedS = if (caseInsensitive) s.toLowerCase else s
    val internedS = if (internStrings) Processor.internString(casedS) else casedS

    internedS
  }

  def add(tokens: Array[String]): Unit = {
    if (tokens != null && tokens.length > 0) {
      // first layer
      val inTokens = tokens.map(in) // Do them all at once so that trie doesn't need to
      val token = inTokens.head
      val tree = entries.get(token).map { tree =>
        // If there was already a completePath, there still is.
        if (tokens.length == 1) tree.completePath = true
        tree
      }
      .getOrElse {
        val tree = new BooleanTrieNode(token, tokens.length == 1)

        entries.put(token, tree)
        tree
      }
      tree.add(inTokens, 1)
    }
  }

  /**
    * Returns the length of the matched span, which may be 0.
    * When multiple paths are found, the longest one is kept
    * Text must be normalized (i.e., case folding) BEFORE this call, if necessary!
    */
  def findAt(sequenceNormalized: Array[String], offset: Int): BooleanTrieNode.Match = {
    val longestMatch = new BooleanTrieNode.Match()

    entries.get(sequenceNormalized(offset)).map { tree =>
      tree.find(sequenceNormalized, offset, 0, longestMatch)
    }
    longestMatch
  }
}

object BooleanTrieNode {
  class Match(var length: Int = 0)
}

@SerialVersionUID(1000L)
case class BooleanTrieNode(token: String, var completePath: Boolean, var children: Option[ListBuffer[BooleanTrieNode]]) extends Serializable {

  def this(token: String, completePath: Boolean) = this(token, completePath, None)

  def toString(stringBuilder: StringBuilder, label: Option[String]): Unit = {
    stringBuilder.append(token)
    if (completePath) {
      stringBuilder.append("*")
      label.foreach(stringBuilder.append)
    }
    children.foreach { children: ListBuffer[BooleanTrieNode] =>
      stringBuilder.append(" (")
      children.zipWithIndex.foreach { case (child: BooleanTrieNode, index: Int) =>
        if (index > 0)
          stringBuilder.append(" | ")
        child.toString(stringBuilder, label)
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
      longestMatch: BooleanTrieNode.Match): Boolean = {
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
        if (completePath && currentSpanLength + 1 > longestMatch.length)
          longestMatch.length = currentSpanLength + 1
        if (children.isDefined)
          children.get.find(_.find(sequence, startOffset, currentSpanLength + 1, longestMatch))
        true
      }
    }
  }

  def add(tokens: Array[String], offset: Int): Unit = {
    if (offset < tokens.length) {
      // Don't necessarily need a new one if it is found in the tree already
      val child = addTokenToTree(tokens(offset), offset == tokens.length - 1)

      child.add(tokens, offset + 1)
    }
  }

  private def addTokenToTree(newToken: String, newCompletePath: Boolean): BooleanTrieNode = {
    if (children.isEmpty)
      children = Some(new ListBuffer[BooleanTrieNode])

    val someChildren = children.get
    val (index, child) = {
      // Attempt not to go through list twice by leaking matching child.
      var foundChild: Option[BooleanTrieNode] = None
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
      if (newCompletePath)
        someChild.completePath = newCompletePath
      someChild
    }
    else {
      val newChild = new BooleanTrieNode(newToken, newCompletePath)

      if (index >= 0)
        someChildren.insert(index, newChild)
      else
        // The new child is lexicographically "higher" than all existing children.
        someChildren += newChild
      newChild
    }
  }
}

class DebugBooleanHashTrie(label: String, caseInsensitive: Boolean = true, internStrings: Boolean = true) extends BooleanHashTrie(label, caseInsensitive, internStrings) {
  val uniqueStrings = new mutable.HashSet[String]()
  var addedTokensCount: Int = 0
  var addedCount: Int = 0
  val tokenHistogram: mutable.HashMap[Int, Int] = new mutable.HashMap()

  def uniqueStringsSize: Int = uniqueStrings.size

  override def in(s: String): String = {
    val result = super.in(s)

    uniqueStrings.add(result)
    result
  }

  def showStats: Unit = {

    //    val childHistogram: mutable.HashMap[Int, Int] = new mutable.HashMap()
    //
    //    def updateHistogram(value: Int): Unit = {
    //      val count = childHistogram.getOrElseUpdate(value, 0)
    //
    //      childHistogram(value) = count + 1
    //    }
    //
    //    def visitTrieNode(trieNode: TrieNode): Unit = {
    //      if (trieNode.children.isDefined) {
    //        updateHistogram(trieNode.children.get.size)
    //        trieNode.children.get.foreach(visitTrieNode)
    //      }
    //      else
    //        updateHistogram(0)
    //    }
    //
    //    entries.values.foreach(visitTrieNode)
    //    childHistogram.foreach { case (childCount, count) =>
    //      println(childCount + "\t" + count)
    //    }
    //
    //    tokenHistogram.foreach { case (tokenCount, count) =>
    //      println(tokenCount + "\t" + count)
    //    }
  }

  override def add(tokens: Array[String]): Unit = {
    super.add(tokens)
    addedTokensCount += tokens.length
    addedCount += 1

    val count = tokenHistogram.getOrElseUpdate(tokens.length, 0)

    tokenHistogram(tokens.length) = count + 1
  }

  /**
    * Generates BIO labels for this sequence when complete trie paths match
    * When multiple paths match, the longest one is kept
    */
  def find(sequence: Array[String], outsideLabel: String): Array[String] = {
    val casedSequence = if (caseInsensitive) sequence.map(_.toLowerCase) else sequence

    findNormalized(casedSequence, outsideLabel)
  }

  private def findNormalized(sequence: Array[String], outsideLabel: String): Array[String] = {
    val labels = new Array[String](sequence.length)
    var offset = 0

    def setNextLabel(label: String): Unit = {
      labels(offset) = label
      offset += 1
    }

    while (offset < sequence.length) {
      val span = findAt(sequence, offset).length

      if (span > 0) {
        setNextLabel(bLabel)
        for (_ <- 1 until span)
          setNextLabel(iLabel)
      }
      else
        setNextLabel(outsideLabel)
    }
    labels
  }
}
