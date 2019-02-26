package org.clulab.struct

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.util.Arrays

import org.clulab.processors.Sentence
import org.clulab.sequences.LexiconNER
import org.clulab.sequences.LexiconNER.OUTSIDE_LABEL

import scala.collection.mutable

@SerialVersionUID(1000L)
class CompactLexiconNER(val labels: Seq[String], val caseInsensitive: Boolean, val headCount: Int,
    val stringIds: Map[String, Int], val parentIsComplete: Array[Int],
    val parentToChild: Array[Int], val childStringIds: Array[Int], val childAsParent: Array[Int],
    knownCaseInsensitives: Set[String], useLemmas: Boolean, val entityValidator: EntityValidator)
    extends LexiconNER(knownCaseInsensitives, useLemmas) {
  protected val bLabels = labels.map("B-" + _)
  protected val iLabels = labels.map("I-" + _)

  def toString(stringBuilder: StringBuilder): Unit = {
    val strings = stringIds.toArray.sortBy(_._2).map(_._1)

    def toString(index: Int, stringId: Int): Unit = {
      val token = strings(stringId)
      stringBuilder.append(token)
      if (parentIsComplete(index) >= 0) {
        stringBuilder.append("*")
        stringBuilder.append(labels(parentIsComplete(index)))
      }
      val firstChild = parentToChild(index)
      val lastChild = parentToChild(index + 1)

      // Alphabetize these so that they match IntHashTrie order.
      if (firstChild < lastChild) {
        val childStringIndexAndTokens: Seq[(Int, String)] = firstChild.until(lastChild).map { childIndex: Int =>
          val token: String = strings(childStringIds(childIndex))
          (childIndex, token)
        }
        val sortedChildStringIndexAndTokens = childStringIndexAndTokens.sortBy(_._2).map(_._1)

        stringBuilder.append(" (")
        sortedChildStringIndexAndTokens.zipWithIndex.foreach { case (childIndex: Int, index: Int) =>
          if (index != 0)
            stringBuilder.append(" | ")
          toString(childAsParent(childIndex), childStringIds(childIndex))
        }
        stringBuilder.append(")")
      }
    }

    0.until(headCount).foreach { index =>
      toString(index, index)
      stringBuilder.append("\n")
    }
  }

  def getLabels: Seq[String] = labels

  class NodeMatch(var span: Int = 0, var index: Int = -1) {
    def reset(): Unit = {
      span = 0
      index = -1
    }
  }

  def find(sentence: Sentence): Array[String] = {
    val rawTokens = getTokens(sentence)
    val tokens = if (caseInsensitive) rawTokens.map(_.toLowerCase) else rawTokens
    val ids = tokens.map(stringIds.getOrElse(_, -1))
    val seq = findLongestMatch(sentence, ids)

    seq
  }

  protected def findLongestMatch(sentence: Sentence, ids: Array[Int]): Array[String] = {
    val labels = new Array[String](ids.length)
    val nodeMatch = new NodeMatch()
    var offset = 0

    def setNextLabel(label: String): Unit = {
      labels(offset) = label
      offset += 1
    }

    while (offset < ids.length) {
      val id = ids(offset)
      val span =
          if (id < 0 || headCount <= id) 0
          else {
            findAt(ids, offset, nodeMatch)
            nodeMatch.span
          }

      if (span > 0) {
        if (contentfulSpan(sentence, offset, span) && // does this look like a valid entity span?
            entityValidator.validMatch(sentence, offset, offset + span)) { // domain-specific constraints on entities
          val bLabel = bLabels(nodeMatch.index)
          val iLabel = iLabels(nodeMatch.index)

          setNextLabel(bLabel)
          for (_ <- 1 until span)
            setNextLabel(iLabel)
        }
        else
          for (_ <- 0 until span)
            setNextLabel(OUTSIDE_LABEL)
        nodeMatch.reset
      }
      else
        setNextLabel(OUTSIDE_LABEL)
    }
    labels
  }

  def findAt(ids: Array[Int], wordIndex: Int, nodeMatch: NodeMatch): Unit = {

    def linearSearch(value: Int, left: Int, right: Int): Int = {
      var index = left

      while (index < right && childStringIds(index) < value)
        index += 1
      if (index == right) -1
      else index
    }

    def binarySearch(value: Int, left: Int, right: Int): Int = {
      Arrays.binarySearch(childStringIds, left, right, value)
    }

    def findMore(parentIndex: Int, wordIndex: Int): Int = {
      if (wordIndex < ids.length) {
        val childStringId = ids(wordIndex)
        val childIndex =
          if (childStringId < 0) -1
          else binarySearch(childStringId, parentToChild(parentIndex), parentToChild(parentIndex + 1))

        if (childIndex >= 0) {
          val newParentIndex = childAsParent(childIndex)
          val tailCount = findMore(newParentIndex, wordIndex + 1)

          if (tailCount > 0) 1 + tailCount
          else if (parentIsComplete(newParentIndex) >= 0) {
            nodeMatch.index = parentIsComplete(newParentIndex)
            1
          }
          else 0
        }
        else 0
      }
      else 0
    }

    val stringId = ids(wordIndex)
    val tailCount = findMore(stringId, wordIndex + 1)

    if (tailCount > 0) nodeMatch.span = 1 + tailCount
    else if (parentIsComplete(stringId) >= 0) {
      nodeMatch.span = 1
      nodeMatch.index = parentIsComplete(stringId)
    }
  }

  def save(out: ObjectOutputStream): Unit = {
    val texts = stringIds.toArray.sortBy(_._2).map(_._1).mkString("\n")

    out.writeObject(labels)
    out.writeObject(bLabels)
    out.writeObject(iLabels)
    out.writeBoolean(caseInsensitive)
    out.writeInt(headCount)
    out.writeObject(texts)
    out.writeObject(parentIsComplete)
    out.writeObject(parentToChild)
    out.writeObject(childStringIds)
    out.writeObject(childAsParent)
    out.writeObject(entityValidator)
  }
}

object CompactLexiconNER {

  def load(in: ObjectInputStream): Any = {
    var labels = in.readObject.asInstanceOf[Seq[String]]
    val caseInsensitive = in.readBoolean
    val headCount = in.readInt()
    val texts = in.readObject.asInstanceOf[String]
    val stringIds = texts.split('\n').zipWithIndex.toMap
    val parentIsComplete = in.readObject.asInstanceOf[Array[Int]]
    val parentToChild = in.readObject.asInstanceOf[Array[Int]]
    val childStringIds = in.readObject.asInstanceOf[Array[Int]]
    val childAsParent = in.readObject.asInstanceOf[Array[Int]]
    val knownCaseInsensitives = in.readObject.asInstanceOf[Set[String]]
    val useLemmas = in.readObject.asInstanceOf[Boolean]
    val entityValidator = in.readObject.asInstanceOf[EntityValidator]

    new CompactLexiconNER(labels, caseInsensitive, headCount,
      stringIds, parentIsComplete, parentToChild, childStringIds, childAsParent,
      knownCaseInsensitives, useLemmas, entityValidator)
  }

  protected def countChildren(trieNode: IntTrieNode): Int = {
    if (trieNode.children.isDefined) {
      val childTrieNodes = trieNode.children.get

      childTrieNodes.size + childTrieNodes.foldLeft(0) { (count, trieNode) => count + countChildren(trieNode) }
    }
    else 0
  }

  def apply(matcher: IntHashTrie, labels: Seq[String], knownCaseInsensitives: Set[String],
      useLemmas: Boolean, entityValidator: EntityValidator): CompactLexiconNER = {
    /** This gives each string a unique integer ID with some encoded information.
      *
      * If the ID is below headCount, then the TrieNode would exist in entries and
      * the TrieNode could start a named entity.  The index into the array of TrieNodes
      * is the stringId.
      *
      * If the ID is headCount or above, then the TrieNode would exist in entries, but
      * it could not start a named entity.  Again, the index into the array of TrieNodes
      * is the stringId.
      *
      * In both the last cases, the index is also used for isComplete.
      */
    val stringIds: mutable.HashMap[String, Int] = new mutable.HashMap()
    val trieNodes = matcher.entries.values.toArray
    val headCount = trieNodes.size
    val childTrieNodeCount = trieNodes.foldLeft(0) { (count, trieNode) => count + countChildren(trieNode)}
    val parentIsComplete: Array[Int] = new Array(trieNodes.length + childTrieNodeCount)
    val parentToChild: Array[Int] = new Array(trieNodes.length + childTrieNodeCount + 1)
    val childStringIds: Array[Int] = new Array(childTrieNodeCount)
    val childAsParent: Array[Int] = new Array(childTrieNodeCount)

    val parentSizes: Array[Int] = new Array(trieNodes.length + childTrieNodeCount)
    Arrays.fill(parentSizes, -1)

    Arrays.fill(parentIsComplete, -1)
    Arrays.fill(parentToChild, -1)
    Arrays.fill(childStringIds, -1)
    Arrays.fill(childAsParent, -1)

    parentToChild(0) = 0 // Ante up

    // Assume that trieNodes are already sorted as much as necessary and all the tokens have stringIds.
    // Returns the number of parentsAdded and childrenAdded
    def add(trieNodes: Array[IntTrieNode], parentOffset: Int, childOffset: Int): (Int, Int) = {
      // Area between parentOffset and parentOffset + parentRserve is for this recursive pass and
      // likewise for between childOffset and childOffset + childReserve.
      val parentReserve = trieNodes.length
      // Count children and make sure all have stringIds
      val childrenReserve = trieNodes.foldLeft(0) { (count: Int, trieNode: IntTrieNode) =>
        trieNode.children.map { trieNodeChildren =>
          // While at it, register tokens of the children.
          trieNodeChildren.foreach { trieNode =>
            stringIds.getOrElseUpdate(trieNode.token, stringIds.size)
          }
          trieNodeChildren.size
        }.getOrElse(0) + count
      }
      // Fill in offsets, isComplete.
      trieNodes.zipWithIndex.foreach { case (trieNode, index) =>
        parentIsComplete(parentOffset + index) = trieNode.completePath
        parentSizes(parentOffset + index) = trieNode.children.map(_.size).getOrElse(0)
        parentToChild(parentOffset + index + 1) = parentToChild(parentOffset + index) + trieNode.children.map(_.size).getOrElse(0)
      }

      // Work on children
      var currentChildOffset = childOffset
      var parentAdded = 0
      var childrenAdded = 0
      trieNodes.foreach { trieNode =>
        if (trieNode.children.isDefined) {
          val unsortedChildren = trieNode.children.get

          unsortedChildren.foreach { trieNode =>
            stringIds.getOrElseUpdate(trieNode.token, stringIds.size)
          }

          val sortedChildren = unsortedChildren.toArray.sortWith { (left, right) =>
            stringIds(left.token) < stringIds(right.token)
          }

          sortedChildren.zipWithIndex.foreach { case (child, index) =>
            childStringIds(currentChildOffset + index) = stringIds(child.token)
            childAsParent(currentChildOffset + index) = parentOffset + parentReserve + parentAdded + index
          }

          var (incParentAdded, incChildrenAdded) = add(sortedChildren, parentOffset + parentReserve + parentAdded, childOffset + childrenReserve + childrenAdded)

          parentAdded += incParentAdded
          childrenAdded += incChildrenAdded
          currentChildOffset += sortedChildren.length
        }
      }
      (parentReserve + parentAdded, childrenReserve + childrenAdded)
    }
    // Make sure these get in there first, because only children are done above.
    trieNodes.foreach { trieNode =>
      stringIds.getOrElseUpdate(trieNode.token, stringIds.size)
    }
    add(trieNodes, 0, 0)
    require(!parentToChild.exists(_ == -1))
    require(!childStringIds.exists(_ == -1))
    require(!childAsParent.exists(_ == -1))
    parentSizes.indices.foreach { index =>
      require(parentSizes(index) == parentToChild(index + 1) - parentToChild(index))
    }

    new CompactLexiconNER(labels, matcher.caseInsensitive, headCount, stringIds.toMap,
        parentIsComplete, parentToChild, childStringIds, childAsParent, knownCaseInsensitives,
        useLemmas, entityValidator)
  }
}
