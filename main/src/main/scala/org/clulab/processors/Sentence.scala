package org.clulab.processors

import org.clulab.struct.{DirectedGraph, GraphMap, RelationTriple, Tree}
import org.clulab.struct.GraphMap._
import org.clulab.utils.SeqUtils

import scala.collection.immutable.Range
import scala.collection.mutable
import scala.util.hashing.MurmurHash3._


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
  var graphs: GraphMap = new GraphMap
  /** Relation triples from OpenIE */
  var relations:Option[Array[RelationTriple]] = None


  def size:Int = raw.length

  def indices: Range = 0 until size

  def ambivalenceHash: Int = cachedAmbivalenceHash

  protected lazy val cachedAmbivalenceHash = calculateAmbivalenceHash

  protected def calculateAmbivalenceHash: Int = {
    val h0 = stringHash(Sentence.getClass.getName)
    val h1 = mix(h0, orderedHash(raw))
    val h2 = mix(h1, orderedHash(startOffsets))
    val h3 = mix(h2, orderedHash(endOffsets))
    finalizeHash(h3, 3)
  }

  /**
    * Used to compare Sentences.
    * @return a hash (Int) based on the contents of a sentence
    */
  def equivalenceHash: Int = {

    val stringCode = "org.clulab.processors.Sentence"

    def getAnnotationsHash(labels: Option[Array[_]]): Int = labels match {
      case Some(lbls) =>
        val h0 = stringHash(s"$stringCode.annotations")
        val hs = lbls.map(_.hashCode)
        val h = mixLast(h0, orderedHash(hs))
        finalizeHash(h, lbls.length)
      case None => None.hashCode
    }

    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash(stringCode)
    // NOTE: words.hashCode will produce inconsistent values
    val h1a = mix(h0, getAnnotationsHash(Some(raw)))
    val h1b = mix(h1a, getAnnotationsHash(Some(words)))
    val h2 = mix(h1b, getAnnotationsHash(Some(startOffsets)))
    val h3 = mix(h2, getAnnotationsHash(Some(endOffsets)))
    val h4 = mix(h3, getAnnotationsHash(tags))
    val h5 = mix(h4, getAnnotationsHash(lemmas))
    val h6 = mix(h5, getAnnotationsHash(entities))
    val h7 = mix(h6, getAnnotationsHash(norms))
    val h8 = mix(h7, getAnnotationsHash(chunks))
    val h9 = mix(h8, if (dependencies.nonEmpty) dependencies.get.equivalenceHash else None.hashCode)
    finalizeHash(h9, 10) 
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
    if(end - start == 1) return raw(start)

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