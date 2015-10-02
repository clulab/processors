package edu.arizona.sista.processors

import edu.arizona.sista.discourse.rstparser.DiscourseTree
import edu.arizona.sista.struct.{Tree, DirectedGraph}
import DependencyMap._

import collection.mutable
import collection.mutable.ListBuffer
import java.lang.StringBuilder

/**
 * Stores all annotations for one document
 * User: mihais
 * Date: 3/1/13
 */
class Document( var id:Option[String],
                val sentences:Array[Sentence],
                var coreferenceChains:Option[CorefChains],
                var discourseTree:Option[DiscourseTree],
                var text:Option[String]) extends Serializable {

  def this(sa:Array[Sentence], cc:Option[CorefChains], dt:Option[DiscourseTree]) {
    this(None, sa, cc, dt, None)
  }

  def this(sa: Array[Sentence]) {
    this(None, sa, None, None, None)
  }

  /** Clears any internal state potentially constructed by the annotators */
  def clear() { }
}

/** Stores the annotations for a single sentence */
class Sentence(
                /** Actual tokens in this sentence */
                val words:Array[String],
                /** Start character offsets for the words; start at 0 */
                val startOffsets:Array[Int],
                /** End character offsets for the words; start at 0 */
                val endOffsets:Array[Int],
                /** POS tags for words */
                var tags:Option[Array[String]],
                /** Lemmas */
                var lemmas:Option[Array[String]],
                /** NE labels */
                var entities:Option[Array[String]],
                /** Normalized values of named/numeric entities, such as dates */
                var norms:Option[Array[String]],
                /** Shallow parsing labels */
                var chunks:Option[Array[String]],
                /** Constituent tree of this sentence; includes head words */
                var syntacticTree:Option[Tree],
                /** DAG of syntactic and semantic dependencies; word offsets start at 0 */
                var dependenciesByType:DependencyMap) extends Serializable {

  def this(
            words:Array[String],
            startOffsets:Array[Int],
            endOffsets:Array[Int]) =
    this(words, startOffsets, endOffsets,
      None, None, None, None, None, None, new DependencyMap)

  def size:Int = words.length

  /**
   * Default dependencies: first Stanford collapsed, then Stanford basic, then None
   * @return A directed graph of dependencies if any exist, otherwise None
   */
  def dependencies:Option[DirectedGraph[String]] = {
    if(dependenciesByType == null) return None

    if(dependenciesByType.contains(STANFORD_COLLAPSED))
      dependenciesByType.get(STANFORD_COLLAPSED)
    else if(dependenciesByType.contains(STANFORD_BASIC))
      dependenciesByType.get(STANFORD_BASIC)
    else
      None
  }

  /** Fetches the Stanford basic dependencies */
  def stanfordBasicDependencies:Option[DirectedGraph[String]] = {
    if(dependenciesByType == null) return None
    dependenciesByType.get(STANFORD_BASIC)
  }

  /** Fetches the Stanford collapsed dependencies */
  def stanfordCollapsedDependencies:Option[DirectedGraph[String]] = {
    if(dependenciesByType == null) return None
    dependenciesByType.get(STANFORD_COLLAPSED)
  }

  def semanticRoles:Option[DirectedGraph[String]] = {
    if(dependenciesByType == null) return None
    dependenciesByType.get(SEMANTIC_ROLES)
  }

  def setDependencies(depType:Int, deps:DirectedGraph[String]): Unit = {
    if(dependenciesByType == null)
      dependenciesByType = new DependencyMap
    dependenciesByType += (depType -> deps)
  }

  /**
   * Recreates the text of the sentence, preserving the original number of white spaces between tokens
   * @return the text of the sentence
   */
  def getSentenceText():String =  getSentenceFragmentText(0, words.length)

  def getSentenceFragmentText(start:Int, end:Int):String = {
    // optimize the single token case
    if(end - start == 1) words(start)

    val text = new mutable.StringBuilder()
    for(i <- start until end) {
      if(i > start) {
        // add as many white spaces as recorded between tokens
        // sometimes this space is negative: in BioNLPProcessor we replace "/" with "and"
        //   in these cases, let's make sure we print 1 space, otherwise the text is hard to read
        val numberOfSpaces = math.max(1, startOffsets(i) - endOffsets(i - 1))
        for (j <- 0 until numberOfSpaces) {
          text.append(" ")
        }
      }
      text.append(words(i))
    }
    text.toString()
  }

}

class DependencyMap extends mutable.HashMap[Int, DirectedGraph[String]] {
  override def initialSize:Int = 2 // we have very few dependency types, so let's create a small hash to save memory
}

object DependencyMap {
  val STANFORD_BASIC = 0 // basic Stanford dependencies
  val STANFORD_COLLAPSED = 1 // collapsed Stanford dependencies
  val SEMANTIC_ROLES = 2 // semantic roles from CoNLL 2008-09, which includes PropBank and NomBank
}

/** Stores a single coreference mention */
class CorefMention (
                     /** Index of the sentence containing this mentions; starts at 0 */
                     val sentenceIndex:Int,
                     /** Token index for the mention head word; starts at 0 */
                     val headIndex:Int,
                     /** Start token offset in the sentence; starts at 0 */
                     val startOffset:Int,
                     /** Offset of token immediately after this mention; starts at 0 */
                     val endOffset:Int,
                     /** Id of the coreference chain containing this mention; -1 if singleton mention */
                     val chainId:Int) extends Serializable {

  def length = endOffset - startOffset

  override def equals(other:Any):Boolean = {
    other match {
      case that:CorefMention =>
        sentenceIndex == that.sentenceIndex &&
        headIndex == that.headIndex &&
        startOffset == that.startOffset &&
        endOffset == that.endOffset
      case _ => false
    }
  }

  override def hashCode = {
    41 * (41 * (41 * (41 + sentenceIndex))) +
      41 * (41 * (41 + headIndex)) +
      41 * (41 + startOffset) +
      endOffset
  }

  override def toString:String = {
    val os = new StringBuilder
    os.append("(")
    os.append(sentenceIndex)
    os.append(", ")
    os.append(headIndex)
    os.append(", ")
    os.append(startOffset)
    os.append(", ")
    os.append(endOffset)
    os.append(")")
    os.toString
  }
}

/** Stores all the coreference chains extracted in one document */
class CorefChains (rawMentions:Iterable[CorefMention]) extends Serializable {

  /**
   * Indexes all mentions in a document by sentence index (starting at 0) and head index (starting at 0)
   * This means we store only one mention per head (unlike CoreNLP which may have multiple)
   * In case a specific processor maintains multiple mentions with same head, we keep the longest
   */
  val mentions:Map[(Int, Int), CorefMention] = CorefChains.mkMentions(rawMentions)

  /**
   * Indexes all coreference chains in a document using a unique id per chain
   * These do not include singleton clusters
   */
  val chains:Map[Int, Iterable[CorefMention]] = CorefChains.mkChains(mentions)

  /** Fetches the mention with this sentence and head indices */
  def getMention(sentenceIndex:Int, headIndex:Int):Option[CorefMention] =
    mentions.get((sentenceIndex, headIndex))

  /** Fetches the coreference chain for the mention with this sentence and head indices; None for singletons */
  def getChain(sentenceIndex:Int, headIndex:Int):Option[Iterable[CorefMention]] = {
    getMention(sentenceIndex, headIndex).foreach(m => return getChain(m))
    None
  }

  /** Fetches the coreference chain for this mention; None for singletons */
  def getChain(mention:CorefMention):Option[Iterable[CorefMention]] = {
    if (mention.chainId == -1) return None
    chains.get(mention.chainId)
  }

  /** All recognized chains, without singletons */
  def getChains:Iterable[Iterable[CorefMention]] = chains.values

  /** All mentions in this document */
  def getMentions:Iterable[CorefMention] = mentions.values

  def isEmpty = mentions.isEmpty && chains.isEmpty
}

object CorefChains {
  private def lessThanForMentions(x:CorefMention, y:CorefMention):Boolean = {
    if (x.sentenceIndex < y.sentenceIndex) return true
    if (x.sentenceIndex > y.sentenceIndex) return false

    if (x.headIndex < y.headIndex) return true
    if (x.headIndex > y.headIndex) return false

    val diffSize = (x.endOffset - x.startOffset) - (y.endOffset - y.startOffset)
    if (diffSize > 0) return true
    if (diffSize < 0) return false

    true
  }

  private def mkMentions(rawMentions:Iterable[CorefMention]):Map[(Int, Int), CorefMention] = {
    // if multiple mentions with same head exist, keep only the longest
    val sortedMentions = rawMentions.toList.sortWith(lessThanForMentions)
    val mentionMap = new mutable.HashMap[(Int, Int), CorefMention]
    var prevMention:CorefMention = null
    for (m <- sortedMentions) {
      // println(m.sentenceIndex + " " + m.headIndex + " " + m.startOffset + " " + m.endOffset)

      // eliminate duplicate mentions (same sentence, same head)
      // if found, we keep the previous, which is guaranteed to be longer due to sorting criterion
      if (prevMention != null &&
        prevMention.sentenceIndex == m.sentenceIndex &&
        prevMention.headIndex == m.headIndex) {
        assert(prevMention.length >= m.length)
      } else {
        mentionMap += (m.sentenceIndex, m.headIndex) -> m
      }

      prevMention = m
    }
    mentionMap.toMap
  }

  private def mkChains(mentions:Map[(Int, Int), CorefMention]):Map[Int, Iterable[CorefMention]] = {
    val chainBuffer = new mutable.HashMap[Int, ListBuffer[CorefMention]]
    for (m <- mentions.values) {
      var cb = chainBuffer.get(m.chainId)
      if (cb.isEmpty) {
        val cbv = new ListBuffer[CorefMention]
        chainBuffer += m.chainId -> cbv
        cb = chainBuffer.get(m.chainId)
        assert(cb.isDefined)
      }
      cb.get += m
    }
    val chainMap = new mutable.HashMap[Int, Iterable[CorefMention]]
    for (cid <- chainBuffer.keySet) {
      chainMap += cid -> chainBuffer.get(cid).get.toList
    }
    chainMap.toMap
  }
}