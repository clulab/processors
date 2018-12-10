package org.clulab.struct

import org.clulab.processors.Processor

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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

  def add(tokens:Array[String]): Unit = {
    if(tokens == null || tokens.length == 0) {
      // nothing to see; move on
      return
    }

    // first layer
    val token = in(tokens.head)
    // If orElse is hit, a new TrieNode is definitely needed.
    val tree = entries.getOrElse(token, new TrieNode(token, tokens.length == 1))

    tree.completePath = tree.completePath || tokens.length == 1
    entries.put(token, tree)
    // following layers
    if(tokens.length > 1) add(tree, tokens, 1)
  }

  private def add(tree:TrieNode, tokens:Array[String], offset:Int): Unit = {
    // Don't necessarily need a new one if it is found in the tree already
    val child = addTokenToTree(tree, new TrieNode(in(tokens(offset)), offset == tokens.length - 1))

    if(offset < tokens.length - 1) {
      add(child, tokens, offset + 1)
    }
  }

  private def addTokenToTree(parent:TrieNode, newChild:TrieNode):TrieNode = {
    val children = parent.children.getOrElse {
      val newChildren = new ListBuffer[TrieNode]
      parent.children = Some(newChildren)
      newChildren
    }

    for(i <- children.indices) {
      val child = children(i)
      val compare = newChild.token.compareTo(child.token)
      if(compare < 0) {
        children.insert(i, newChild) // Now is when a new one is needed
        return newChild
      } else if(compare == 0) {
        // this node already exists; just adjust the complete path flag, if necessary
        child.completePath = child.completePath || newChild.completePath
        return child
      }
    }
    // the new child is lexicographically "higher" than all existing children
    children += newChild
    newChild
  }

  /**
    * Generates BIO labels for this sequence when complete trie paths match
    * When multiple paths match, the longest one is kept
    */
  def find(sequence:Array[String], label:String, outsideLabel:String):Array[String] = {
    val casedSequence = if (caseInsensitive) sequence.map(_.toLowerCase) else sequence

    findNormalized(casedSequence, label, outsideLabel)
  }

  private def findNormalized(sequence:Array[String], label:String, outsideLabel:String):Array[String] = {
    val labels = new Array[String](sequence.length)
    var offset = 0
    while(offset < sequence.length) {
      val span = findAt(sequence, offset)
      if (span > 0) {
        labels(offset) = "B-" + label
        offset += 1
        for (i <- 1 until span) {
          labels(offset) = "I-" + label
          offset += 1
        }
      } else {
        labels(offset) = outsideLabel
        offset += 1
      }
    }
    labels
  }

  /**
   * Returns the length of the matched span, which may be 0.
   * When multiple paths are found, the longest one is kept
   * Text must be normalized (i.e., case folding) BEFORE this call, if necessary!
   */
  def findAt(sequenceNormalized:Array[String], offset:Int):Int = {
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

    if (startOffset + currentSpanLength >= sequence.length)
      true
    else {
      val comp = sequence(startOffset + currentSpanLength).compareTo(token)

      if (comp < 0)
        true // This still seems backwards
      else if (comp > 0)
        false
      else {
        if (completePath && currentSpanLength + 1 > longestMatch.value)
          longestMatch.value = currentSpanLength + 1

        if (children.isDefined) {
          var shouldStop = false
          for (child <- children.get if !shouldStop) { // Does not actually stop, but continues
            shouldStop = child.find(sequence, startOffset, currentSpanLength + 1, longestMatch)
          }
        }
        true
      }
    }
  }
}
