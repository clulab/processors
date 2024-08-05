package org.clulab.processors

import org.clulab.scala.WrappedArray._
import org.clulab.struct.{DirectedGraph, GraphMap, RelationTriple, Tree}
import org.clulab.struct.GraphMap._
import org.clulab.utils.Hash
import org.clulab.utils.SeqUtils

import scala.collection.mutable

/** Stores the annotations for a single sentence */
class Sentence(
  /** Raw tokens in this sentence; these MUST match the original text */
  val raw: Array[String],
  /** Start character offsets for the raw tokens; start at 0 */
  val startOffsets: Array[Int],
  /** End character offsets for the raw tokens; start at 0 */
  val endOffsets: Array[Int],

  /**
    * Words produced from raw tokens, closer to what the downstream components expect
    * These MAY differ from raw tokens,
    *   e.g., Unicode characters in raw are replaced with ASCII strings, and parens are replaced with -LRB-, -RRB-, etc.
    * However, the number of raw tokens MUST always equal the number of words, so if the exact text must be recovered,
    *   please use the raw tokens with the same positions
    */
  val words: Array[String]) extends Serializable {

  /** POS tags for words */
  var tags: Option[Array[String]] = None
  /** Lemmas */
  var lemmas: Option[Array[String]] = None
  /** NE labels */
  var entities: Option[Array[String]] = None
  /** Normalized values of named/numeric entities, such as dates */
  var norms: Option[Array[String]] = None
  /** Shallow parsing labels */
  var chunks: Option[Array[String]] = None
  /** Constituent tree of this sentence; includes head words */
  var syntacticTree: Option[Tree] = None
  /** DAG of syntactic and semantic dependencies; word offsets start at 0 */
  var graphs: GraphMap = GraphMap()
  /** Relation triples from OpenIE */
  var relations:Option[Array[RelationTriple]] = None


  def size:Int = raw.length

  def indices: Range = 0 until size

  def ambivalenceHash: Int = cachedAmbivalenceHash

  protected lazy val cachedAmbivalenceHash = calculateAmbivalenceHash

  protected def calculateAmbivalenceHash: Int = Hash(
    Hash(Sentence.getClass.getName),
    Hash.ordered(raw),
    Hash.ordered(startOffsets),
    Hash.ordered(endOffsets)
  )

  /**
    * Used to compare Sentences.
    * @return a hash (Int) based on the contents of a sentence
    */
  def equivalenceHash: Int = {
    val stringCode = "org.clulab.processors.Sentence"

    def getAnnotationsHash(labelsOpt: Option[Array[_]]): Int = labelsOpt
        .map { labels =>
          val hs = labels.map(_.hashCode)
          val result = Hash.withLast(labels.length)(
            Hash(s"$stringCode.annotations"),
            Hash.ordered(hs)
          )
          
          result
        }
        .getOrElse(None.hashCode)

    Hash(
      Hash(stringCode),
      getAnnotationsHash(Some(raw)),
      getAnnotationsHash(Some(words)),
      getAnnotationsHash(Some(startOffsets)),
      getAnnotationsHash(Some(endOffsets)),
      getAnnotationsHash(tags),
      getAnnotationsHash(lemmas),
      getAnnotationsHash(entities),
      getAnnotationsHash(norms),
      getAnnotationsHash(chunks),
      if (dependencies.nonEmpty) dependencies.get.equivalenceHash else None.hashCode
    )
  }

  /**
    * Default dependencies: first Universal enhanced, then Universal basic, then None
    *
    * @return A directed graph of dependencies if any exist, otherwise None
    */
  def dependencies:Option[DirectedGraph[String]] = graphs match {
    case collapsed if collapsed.contains(UNIVERSAL_ENHANCED) => collapsed.get(UNIVERSAL_ENHANCED)
    case basic if basic.contains(UNIVERSAL_BASIC) => basic.get(UNIVERSAL_BASIC)
    case _ => None
  }

  /** Fetches the universal basic dependencies */
  def universalBasicDependencies:Option[DirectedGraph[String]] = graphs.get(UNIVERSAL_BASIC)

  /** Fetches the universal enhanced dependencies */
  def universalEnhancedDependencies:Option[DirectedGraph[String]] = graphs.get(UNIVERSAL_ENHANCED)

  /** Fetches the Stanford basic dependencies */
  def stanfordBasicDependencies:Option[DirectedGraph[String]] = graphs.get(STANFORD_BASIC)

  /** Fetches the Stanford collapsed dependencies */
  def stanfordCollapsedDependencies:Option[DirectedGraph[String]] = graphs.get(STANFORD_COLLAPSED)

  def semanticRoles:Option[DirectedGraph[String]] = graphs.get(SEMANTIC_ROLES)
  def enhancedSemanticRoles:Option[DirectedGraph[String]] = graphs.get(ENHANCED_SEMANTIC_ROLES)

  def hybridDependencies:Option[DirectedGraph[String]] = graphs.get(HYBRID_DEPENDENCIES)

  def setDependencies(depType: String, deps: DirectedGraph[String]): Unit = graphs += (depType -> deps)

  /**
    * Recreates the text of the sentence, preserving the original number of white spaces between tokens
    *
    * @return the text of the sentence
    */
  def getSentenceText:String =  getSentenceFragmentText(0, words.length)

  def getSentenceFragmentText(start:Int, end:Int):String = {
    // optimize the single token case
    if (end - start == 1) raw(start)
    else {
      val text = new mutable.StringBuilder()
      for(i <- start until end) {
        if(i > start) {
          // add as many white spaces as recorded between tokens
          val numberOfSpaces = math.max(1, startOffsets(i) - endOffsets(i - 1))
          for (j <- 0 until numberOfSpaces) {
            text.append(" ")
          }
        }
        text.append(raw(i))
      }
      text.toString()
    }
  }

  /** Reverts the current sentence */
  def revert():Sentence = {
    val reverted = new Sentence(
      SeqUtils.revert(raw).toArray,
      SeqUtils.revert(startOffsets).toArray,
      SeqUtils.revert(endOffsets).toArray,
      SeqUtils.revert(words).toArray)
    if(tags.nonEmpty)
      reverted.tags = Some(SeqUtils.revert(tags.get).toArray)
    if(lemmas.nonEmpty)
      reverted.lemmas = Some(SeqUtils.revert(lemmas.get).toArray)
    if(entities.nonEmpty)
      reverted.entities = Some(SeqUtils.revert(entities.get).toArray)
    if(norms.nonEmpty)
      reverted.norms = Some(SeqUtils.revert(norms.get).toArray)
    if(chunks.nonEmpty)
      reverted.chunks = Some(SeqUtils.revert(chunks.get).toArray)

    // TODO: revert syntacticTree and graphs!

    reverted
  }

  def assimilate(sentence: Sentence): Sentence = {
    tags = sentence.tags
    lemmas = sentence.lemmas
    entities = sentence.entities
    norms = sentence.norms
    chunks = sentence.chunks
    syntacticTree = sentence.syntacticTree
    graphs = sentence.graphs
    relations = sentence.relations
    this
  }

  def copy(raw: Array[String] = raw, startOffsets: Array[Int] = startOffsets, endOffsets: Array[Int] = endOffsets, words: Array[String] = words): Sentence =
      new Sentence(raw, startOffsets, endOffsets, words).assimilate(this)

  def offset(offset: Int): Sentence = {
    if (offset == 0) this
    else {
      val newStartOffsets = startOffsets.map(_ + offset).toArray
      val newEndOffsets = endOffsets.map(_ + offset).toArray

      copy(startOffsets = newStartOffsets, endOffsets = newEndOffsets)
    }
  }
}

object Sentence {

  def apply(
    raw:Array[String],
    startOffsets: Array[Int],
    endOffsets: Array[Int]): Sentence =
    new Sentence(raw, startOffsets, endOffsets, raw) // words are identical to raw tokens (a common situation)

  def apply(
    raw:Array[String],
    startOffsets: Array[Int],
    endOffsets: Array[Int],
    words: Array[String]): Sentence =
    new Sentence(raw, startOffsets, endOffsets, words)

  def apply(
    raw: Array[String],
    startOffsets: Array[Int],
    endOffsets: Array[Int],
    words: Array[String],
    tags: Option[Array[String]],
    lemmas: Option[Array[String]],
    entities: Option[Array[String]],
    norms: Option[Array[String]],
    chunks: Option[Array[String]],
    tree: Option[Tree],
    deps: GraphMap,
    relations: Option[Array[RelationTriple]]
  ): Sentence = {
    val s = Sentence(raw, startOffsets, endOffsets, words)
    // update annotations
    s.tags = tags
    s.lemmas = lemmas
    s.entities = entities
    s.norms = norms
    s.chunks = chunks
    s.syntacticTree = tree
    s.graphs = deps
    s.relations = relations
    s
  }

}