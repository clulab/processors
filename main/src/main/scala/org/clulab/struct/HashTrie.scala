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
class HashTrie(val caseInsensitive:Boolean = true, val internStrings:Boolean = true) extends Serializable {

  /** Stores the first layer, i.e., the entry points in the trie */
  val entries = new mutable.HashMap[String, TrieNode]()

  /** For stats */
  val uniqueStrings = new mutable.HashSet[String]()

  override def toString:String = entries.values.mkString("", "\n", "\n")

  def in(s:String):String = {
    val casedS = if (caseInsensitive) s.toLowerCase else s
    val internedS = if (internStrings) Processor.internString(casedS) else casedS

    uniqueStrings.add(internedS)
    internedS
  }

  def add(tokens: Array[String]): Unit = {
    if (tokens != null && tokens.length > 0) {
      // first layer
      val inTokens = tokens.map(in) // Do them all at once so that trie doesn't need to
      val token = inTokens.head
      val tree = entries.get(token).map { tree =>
        tree.completePath = tree.completePath || tokens.length == 1
        tree
      }.getOrElse {
        val tree = new TrieNode(token, tokens.length == 1)

        entries.put(token, tree)
        tree
      }

      tree.add(inTokens, 1)
    }
  }

  /**
    * Generates BIO labels for this sequence when complete trie paths match
    * When multiple paths match, the longest one is kept
    */
  def find(sequence:Array[String], label:String, outsideLabel:String): Array[String] = {
    val casedSequence = if (caseInsensitive) sequence.map(_.toLowerCase) else sequence

    findNormalized(casedSequence, label, outsideLabel)
  }

  private def findNormalized(sequence:Array[String], label:String, outsideLabel:String): Array[String] = {
    lazy val bLabel = "B-" + label
    lazy val iLabel = "I-" + label
    val labels = new Array[String](sequence.length)

    var offset = 0
    def setNextLabel(value: String): Unit = {
      labels(offset) = value
      offset += 1
    }

    // TODO, the labels shouldn't need to be built each time.  They can be specified in the constructor.
    // Then also don't need to keep track of them in a tuple?

    while (offset < sequence.length) {
      val span = findAt(sequence, offset)

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

  /**
   * Returns the length of the matched span, which may be 0.
   * When multiple paths are found, the longest one is kept
   * Text must be normalized (i.e., case folding) BEFORE this call, if necessary!
   */
  def findAt(sequenceNormalized: Array[String], offset: Int):Int = {
    entries.get(sequenceNormalized(offset)).map { tree =>
      val longestMatch = new MutableNumber[Int](0)

      tree.find(sequenceNormalized, offset, 0, longestMatch)
      longestMatch.value
    }.getOrElse(0)
  }
}

@SerialVersionUID(1000L)
case class TrieNode(token:String, var completePath:Boolean, var children:Option[ListBuffer[TrieNode]]) extends Serializable {

  def this(token: String, complete: Boolean) = this(token, complete, None)

  override def toString: String = {
    val os = new StringBuilder
    os.append(token)
    if (completePath) os.append("*")
    children.foreach(cs => os.append(cs.mkString(" (", " | ", ")")))
    os.toString()
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
           longestMatch: MutableNumber[Int]): Boolean = {
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
        if (completePath && currentSpanLength + 1 > longestMatch.value)
          longestMatch.value = currentSpanLength + 1
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

  private def addTokenToTree(newToken: String, newCompletePath: Boolean): TrieNode = {
    if (children.isEmpty)
      children = Some(new ListBuffer[TrieNode])

    val someChildren = children.get
    val (index, child) = {
      // Attempt not to go through list twice by leaking matching child.
      var foundChild: Option[TrieNode] = None
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
      someChild.completePath = someChild.completePath || newCompletePath
      someChild
    }
    else {
      val newChild = new TrieNode(newToken, newCompletePath)

      if (index >= 0)
        someChildren.insert(index, newChild)
      else
        // The new child is lexicographically "higher" than all existing children.
        someChildren += newChild
      newChild
    }
  }
}
