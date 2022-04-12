package org.clulab.sequences

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.util.Arrays

import org.clulab.processors.Sentence
import org.clulab.sequences.LexiconNER.OUTSIDE_LABEL
import org.clulab.struct.EntityValidator
import org.clulab.struct.IntHashTrie
import org.clulab.struct.IntTrieNode

import scala.collection.mutable

/** Lexicon-based NER similar to [[org.clulab.sequences.CombinedLexiconNER CombinedLexiconNER]] but which
  * also adds efficient serialization, deserialization, and storage by using the
  * [[org.clulab.sequences.CompactTrie CompactTrie]]
  *
  * @param caseInsensitiveMatcher A CompactTrie to be matched for for case insensitive KBs
  * @param caseSensitiveMatcher A CompactTrie to be matched for for case sensitive KBs
  * @param labels Labels matching all of the kbs and overrideKBs used in the matchers.  They
  * should be in the order that the kbs were specified and continue in the order that any
  * additional labels are encountered in overrideKBs.
  * @param knownCaseInsensitives Set of single-token entity names that can be spelled using
  * lower case, according to the KB(s)
  * @param useLemmas If true, tokens are matched using lemmas, otherwise using words
  * @param entityValidator An object able to validate any matches that are found
  */
@SerialVersionUID(1000L)
// These are var for serialization optimization and then only to preserve compatibility with other LexiconNERs.
class CompactLexiconNER(
  var caseInsensitiveCompactTrie: CompactTrie,
  var caseSensitiveCompactTrie: CompactTrie,
  var labels: Seq[String], knownCaseInsensitives: Set[String], useLemmas: Boolean, var entityValidator: EntityValidator
) extends LexiconNER(knownCaseInsensitives, useLemmas) {
  // Make sure they arrive in the right order.
  require(caseInsensitiveCompactTrie.caseInsensitive)
  require(!caseSensitiveCompactTrie.caseInsensitive)

  @transient protected var bLabels: Seq[String] = mkBLabels
  @transient protected var iLabels: Seq[String] = mkILabels

  protected def mkBLabels: Seq[String] = labels.map("B-" + _)

  protected def mkILabels: Seq[String] = labels.map("I-" + _)

  protected def hasCaseInsensitive: Boolean = caseInsensitiveCompactTrie.nonEmpty

  protected def hasCaseSensitive: Boolean = caseSensitiveCompactTrie.nonEmpty

  def toString(stringBuilder: StringBuilder): Unit = {
    caseInsensitiveCompactTrie.toString(stringBuilder, labels)
    stringBuilder.append("\n")
    caseSensitiveCompactTrie.toString(stringBuilder, labels)
  }

  def getLabels: Seq[String] = labels

  def find(sentence: Sentence): Array[String] = {
    val caseSensitiveTokens = getTokens(sentence)
    val caseInsensitiveTokens =
      if (hasCaseInsensitive) caseSensitiveTokens.map(_.toLowerCase)
      else caseSensitiveTokens
    val seq = findLongestMatch(sentence, caseSensitiveTokens, caseInsensitiveTokens)

    seq
  }

  protected def findLongestMatch(sentence: Sentence, caseSensitiveTokens: Array[String],
      caseInsensitiveTokens: Array[String]): Array[String] = {
    val labels = new Array[String](caseSensitiveTokens.length)
    val length = labels.length
    var offset = 0

    val   caseSensitiveStringIds = if (hasCaseSensitive)     caseSensitiveTokens.map(  caseSensitiveCompactTrie.stringIds) else Array.empty[Int]
    val caseInsensitiveStringIds = if (hasCaseInsensitive) caseInsensitiveTokens.map(caseInsensitiveCompactTrie.stringIds) else Array.empty[Int]

    // These are intended to cut down on the number of objects created.
    // It worked better when there was only one setting for case.
    val spanAndIndex = new SpanAndIndex()
    val caseSensitiveSpanAndIndex   = new SpanAndIndex()
    val caseInsensitiveSpanAndIndex = new SpanAndIndex()

    def setNextLabel(label: String): Unit = {
      labels(offset) = label
      offset += 1
    }

    def updateSpanAndIndex(): Unit = {

      def innerGetSpanAndIndex(condition: Boolean, stringIds: Array[Int], spanAndIndex: SpanAndIndex,
          compactTrie: CompactTrie): SpanAndIndex = {
        if (condition) {
          val id = stringIds(offset)
          if (id < 0 || compactTrie.headCount <= id)
            CompactLexiconNER.noSpanAndIndex
          else {
            spanAndIndex.reset()
            findAt(stringIds, offset, spanAndIndex, compactTrie)
            spanAndIndex
          }
        }
        else CompactLexiconNER.noSpanAndIndex
      }

      val newCaseInsensitiveSpanAndIndex = innerGetSpanAndIndex(hasCaseInsensitive, caseInsensitiveStringIds, caseInsensitiveSpanAndIndex, caseInsensitiveCompactTrie)
      val newCaseSensitiveSpanAndIndex   = innerGetSpanAndIndex(hasCaseSensitive,   caseSensitiveStringIds,   caseSensitiveSpanAndIndex,   caseSensitiveCompactTrie)
      val betterNodeMatch = newCaseInsensitiveSpanAndIndex.orBetter(newCaseSensitiveSpanAndIndex)

      spanAndIndex.reset(betterNodeMatch)
    }

    while (offset < length) {
      val span = {
        updateSpanAndIndex()
        spanAndIndex.span
      }

      if (span > 0) {
        if (contentfulSpan(sentence, offset, span) && // does this look like a valid entity span?
            entityValidator.validMatch(sentence, offset, offset + span)) { // domain-specific constraints on entities
          val bLabel = bLabels(spanAndIndex.index)
          val iLabel = iLabels(spanAndIndex.index)

          setNextLabel(bLabel)
          for (_ <- 1 until span)
            setNextLabel(iLabel)
        }
        else
          for (_ <- 0 until span)
            setNextLabel(OUTSIDE_LABEL)
      }
      else
        setNextLabel(OUTSIDE_LABEL)
    }
    labels
  }

  def findAt(ids: Array[Int], wordIndex: Int, nodeMatch: SpanAndIndex, compactTrie: CompactTrie): Unit = {

    def linearSearch(value: Int, left: Int, right: Int): Int = {
      var index = left

      while (index < right && compactTrie.childStringIds(index) < value)
        index += 1
      if (index == right) -1
      else index
    }

    def binarySearch(value: Int, left: Int, right: Int): Int = {
      Arrays.binarySearch(compactTrie.childStringIds, left, right, value)
    }

    def findMore(parentIndex: Int, wordIndex: Int): Int = {
      if (wordIndex < ids.length) {
        val childStringId = ids(wordIndex)
        val childIndex =
          if (childStringId < 0) -1
          else binarySearch(childStringId, compactTrie.parentToChild(parentIndex), compactTrie.parentToChild(parentIndex + 1))

        if (childIndex >= 0) {
          val newParentIndex = compactTrie.childAsParent(childIndex)
          val tailCount = findMore(newParentIndex, wordIndex + 1)

          if (tailCount > 0) 1 + tailCount
          else if (compactTrie.parentIsComplete(newParentIndex) >= 0) {
            nodeMatch.index = compactTrie.parentIsComplete(newParentIndex)
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
    else if (compactTrie.parentIsComplete(stringId) >= 0) {
      nodeMatch.span = 1
      nodeMatch.index = compactTrie.parentIsComplete(stringId)
    }
  }

  @throws(classOf[java.io.IOException])
  private def writeObject(out: ObjectOutputStream): Unit = {
    out.writeObject(labels)
    out.writeObject(entityValidator)
    out.writeObject(caseInsensitiveCompactTrie)
    out.writeObject(caseSensitiveCompactTrie)
  }

  @throws(classOf[java.io.IOException])
  private def readObject(in: ObjectInputStream): Unit = {
    labels = in.readObject.asInstanceOf[Seq[String]]
    entityValidator = in.readObject.asInstanceOf[EntityValidator]
    caseInsensitiveCompactTrie = in.readObject.asInstanceOf[CompactTrie]
    caseSensitiveCompactTrie   = in.readObject.asInstanceOf[CompactTrie]

    bLabels = mkBLabels
    iLabels = mkILabels
  }
}

// The var here is an attempt at runtime optimization.
class SpanAndIndex(var span: Int = 0, var index: Int = -1) {

  def reset(): Unit = {
    span = 0
    index = -1
  }

  def reset(other: SpanAndIndex): Unit = {
    span = other.span
    index = other.index
  }

  def orBetter(other: SpanAndIndex): SpanAndIndex = {
    // Ties should never occur because the indexes in the two trees should not overlap.
    // Each KB is either case sensitive or not.  The same KB index can't be in both places.
    if (span != other.span)
      // We want the highest value for the span.
      if (span > other.span) this
      else other
    else
      // We want the lowest value for the index.
      if (index <= other.index) this
      else other
  }
}

@SerialVersionUID(1000L)
class CompactTrie(
  var caseInsensitive: Boolean,
  var headCount: Int, stringIdsWithoutDefault: Map[String, Int], var parentIsComplete: Array[Int],
  var parentToChild: Array[Int], var childStringIds: Array[Int], var childAsParent: Array[Int]
) extends Serializable {
  @transient var stringIds: Map[String, Int] = stringIdsWithoutDefault.withDefaultValue(CompactTrie.unknownStringId)

  def nonEmpty: Boolean = headCount > 0

  def isEmpty: Boolean = !nonEmpty

  def toString(stringBuilder: StringBuilder, labels: Seq[String]): Unit = {
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

  @throws(classOf[java.io.IOException])
  private def writeObject(out: ObjectOutputStream): Unit = {
    val texts = stringIds.toArray.sortBy(_._2).map(_._1).mkString("\n")

    out.writeBoolean(caseInsensitive)
    out.writeInt(headCount)
    out.writeObject(texts)
    out.writeObject(parentIsComplete)
    out.writeObject(parentToChild)
    out.writeObject(childStringIds)
    out.writeObject(childAsParent)
  }

  @throws(classOf[java.io.IOException])
  private def readObject(in: ObjectInputStream): Unit = {
    caseInsensitive = in.readBoolean
    headCount = in.readInt()
    val texts = in.readObject.asInstanceOf[String]
    stringIds = texts.split('\n').zipWithIndex.toMap.withDefaultValue(CompactTrie.unknownStringId)
    parentIsComplete = in.readObject.asInstanceOf[Array[Int]]
    parentToChild = in.readObject.asInstanceOf[Array[Int]]
    childStringIds = in.readObject.asInstanceOf[Array[Int]]
    childAsParent = in.readObject.asInstanceOf[Array[Int]]
  }
}

object CompactTrie {
  val unknownStringId: Int = -1

  def apply(
    caseInsensitive: Boolean,
    headCount: Int, stringIds: Map[String, Int], parentIsComplete: Array[Int],
    parentToChild: Array[Int], childStringIds: Array[Int], childAsParent: Array[Int]
  ): CompactTrie = {
    new CompactTrie(caseInsensitive, headCount, stringIds, parentIsComplete, parentToChild, childStringIds, childAsParent)
  }
}

object CompactLexiconNER {
  protected val noSpanAndIndex: SpanAndIndex = new SpanAndIndex()

  protected def countChildren(trieNode: IntTrieNode): Int = {
    if (trieNode.children.isDefined) {
      val childTrieNodes = trieNode.children.get

      childTrieNodes.size + childTrieNodes.foldLeft(0) { (count, trieNode) => count + countChildren(trieNode) }
    }
    else 0
  }

  def apply(caseInsensitiveMatcher: IntHashTrie, caseSensitiveMatcher: IntHashTrie, labels: Seq[String], knownCaseInsensitives: Set[String],
      useLemmas: Boolean, entityValidator: EntityValidator): CompactLexiconNER = {
    // Make sure they arrive in the right order.
    require(caseInsensitiveMatcher.caseInsensitive)
    require(!caseSensitiveMatcher.caseInsensitive)
    val caseInsensitiveCompactTrie = compact(caseInsensitiveMatcher)
    val caseSensitiveCompactTrie = compact(caseSensitiveMatcher)

    new CompactLexiconNER(caseInsensitiveCompactTrie, caseSensitiveCompactTrie,
        labels, knownCaseInsensitives, useLemmas, entityValidator)
  }

  protected def compact(matcher: IntHashTrie): CompactTrie = {
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
    val headCount = trieNodes.length
    val childTrieNodeCount = trieNodes.foldLeft(0) { (count, trieNode) => count + countChildren(trieNode)}
    val parentIsComplete: Array[Int] = new Array(headCount + childTrieNodeCount)
    val parentToChild: Array[Int] = new Array(headCount + childTrieNodeCount + 1)
    val childStringIds: Array[Int] = new Array(childTrieNodeCount)
    val childAsParent: Array[Int] = new Array(childTrieNodeCount)

    val parentSizes: Array[Int] = new Array(headCount + childTrieNodeCount)
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
    require(!parentToChild.contains(-1))
    require(!childStringIds.contains(-1))
    require(!childAsParent.contains(-1))
    parentSizes.indices.foreach { index =>
      require(parentSizes(index) == parentToChild(index + 1) - parentToChild(index))
    }
    CompactTrie(matcher.caseInsensitive, headCount, stringIds.toMap, parentIsComplete, parentToChild, childStringIds, childAsParent)
  }
}
