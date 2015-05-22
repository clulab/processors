package edu.arizona.sista.struct

import edu.arizona.sista.processors.Processor

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * A more efficient trie implementation, where the first layer is stored as a hash map for efficiency (the rest are the usual trees)
 * User: mihais
 * Date: 5/12/15
 */
class HashTrie(val caseInsensitive:Boolean = true, val internStrings:Boolean = true) {
  /** Stores the first layer, i.e., the entry points in the trie */
  val entries = new mutable.HashMap[String, TrieNode]()

  /** For stats */
  val uniqueStrings = new mutable.HashSet[String]()

  override def toString:String = {
    val os = new StringBuilder()
    for(tree <- entries.values) {
      os.append(tree)
      os.append("\n")
    }
    os.toString()
  }

  def in(s:String):String = {
    var ns = caseInsensitive match {
      case true => s.toLowerCase
      case _ => s
    }
    if(internStrings) ns = Processor.internString(ns)
    uniqueStrings.add(ns)
    ns
  }

  def add(tokens:Array[String]) {
    if(tokens == null || tokens.length == 0) {
      // nothing to see; move on
      return
    }

    // first layer
    val token = in(tokens(0))
    val tree = entries.getOrElse(token, new TrieNode(token, tokens.length == 1))
    tree.completePath = tree.completePath || tokens.length == 1
    entries.put(token, tree)

    // following layers
    if(tokens.length > 1) add(tree, tokens, 1)
  }

  private def add(tree:TrieNode, tokens:Array[String], offset:Int): Unit = {
    val child = addTokenToTree(tree, new TrieNode(in(tokens(offset)), offset == tokens.length - 1))

    if(offset < tokens.length - 1) {
      add(child, tokens, offset + 1)
    }
  }

  private def addTokenToTree(parent:TrieNode, newChild:TrieNode):TrieNode = {
    if(parent.children == None) parent.children = Some(new ListBuffer[TrieNode])
    // keep children in alphabetical order
    val children = parent.children.get
    for(i <- 0 until children.size) {
      val child = children(i)
      val compare = newChild.token.compareTo(child.token)
      if(compare < 0) {
        children.insert(i, newChild)
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
    if(caseInsensitive) {
      findNormalized(sequence.map(_.toLowerCase), label, outsideLabel)
    } else {
      findNormalized(sequence, label, outsideLabel)
    }
  }

  private def findNormalized(sequence:Array[String], label:String, outsideLabel:String):Array[String] = {
    var offset = 0
    val labels = new ArrayBuffer[String]()
    while(offset < sequence.length) {
      val span = findAt(sequence, offset)
      if(span > 0) {
        labels += "B-" + label
        for(i <- 1 until span) {
          labels += "I-" + label
        }
        offset += span
      } else {
        labels += outsideLabel
        offset += 1
      }
    }
    labels.toArray
  }

  /**
   * Returns the length of the matched span, or -1 if nothing matched
   * When multiple paths are found, the longest one is kept
   * Text must be normalized (i.e., case folding) BEFORE this call, if necessary!
   */
  def findAt(sequenceNormalized:Array[String], offset:Int):Int = {
    if(! entries.contains(sequenceNormalized(offset))) {
      return -1 // first token in the sequence does not exist in the first layer
    }

    val tree = entries.get(sequenceNormalized(offset)).get
    val longestMatch = new MutableNumber[Int](-1)
    if(tree.completePath) longestMatch.value = 1

    if(tree.children.isDefined) {
      var shouldStop = false
      for (child <- tree.children.get if ! shouldStop) {
        shouldStop = child.find(sequenceNormalized, offset + 1, 1, longestMatch)
      }
    }

    longestMatch.value
  }
}

case class TrieNode(token:String, var completePath:Boolean, var children:Option[ListBuffer[TrieNode]]) {
  def this(token:String, complete:Boolean) = this(token, complete, None)

  override def toString:String = {
    val os = new StringBuilder
    os.append(token)
    if(completePath) os.append("*")
    children.foreach(cs => os.append(" (" + cs.mkString(" | ") + ")"))
    os.toString()
  }

  def find(sequence:Array[String], offset:Int, currentSpanLength:Int, longestMatch:MutableNumber[Int]):Boolean = {
    if(offset >= sequence.length) {
      return true
    }

    val comp = sequence(offset).compareTo(token)
    if(comp < 0) {
      // we can stop here, since children are sorted alphabetically
      return true
    }

    // the text matches this node; so far so good
    if(comp == 0) {
      // this is a complete path
      if(completePath && currentSpanLength > longestMatch.value) {
        longestMatch.value = currentSpanLength + 1
      }

      // continue matching along the children
      if(children.isDefined) {
        var shouldStop = false
        for(child <- children.get if ! shouldStop) {
          shouldStop = child.find(sequence, offset + 1, currentSpanLength + 1, longestMatch)
        }
      }

      // we found the token; no need to continue
      return true
    }

    false
  }
}