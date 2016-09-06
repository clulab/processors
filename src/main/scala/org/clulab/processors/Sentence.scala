package org.clulab.processors

import org.clulab.struct.{DirectedGraph, GraphMap, Tree}
import org.clulab.struct.GraphMap._
import scala.collection.mutable


/** Stores the annotations for a single sentence */
class Sentence(
  /** Actual tokens in this sentence */
  val words: Array[String],
  /** Start character offsets for the words; start at 0 */
  val startOffsets: Array[Int],
  /** End character offsets for the words; start at 0 */
  val endOffsets: Array[Int]
) extends Serializable {

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
  var dependenciesByType: GraphMap = new GraphMap

  def size:Int = words.length

  /**
    * Default dependencies: first Stanford collapsed, then Stanford basic, then None
    *
    * @return A directed graph of dependencies if any exist, otherwise None
    */
  def dependencies:Option[DirectedGraph[String]] = dependenciesByType match {
    case collapsed if collapsed.contains(STANFORD_COLLAPSED) => collapsed.get(STANFORD_COLLAPSED)
    case basic if basic.contains(STANFORD_BASIC) => basic.get(STANFORD_BASIC)
    case _ => None
  }

  /** Fetches the Stanford basic dependencies */
  def stanfordBasicDependencies:Option[DirectedGraph[String]] = dependenciesByType.get(STANFORD_BASIC)

  /** Fetches the Stanford collapsed dependencies */
  def stanfordCollapsedDependencies:Option[DirectedGraph[String]] = dependenciesByType.get(STANFORD_COLLAPSED)

  def semanticRoles:Option[DirectedGraph[String]] = dependenciesByType.get(SEMANTIC_ROLES)

  def setDependencies(depType: String, deps: DirectedGraph[String]): Unit = dependenciesByType += (depType -> deps)

  /**
    * Recreates the text of the sentence, preserving the original number of white spaces between tokens
    *
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

object Sentence {

  def apply(
    words: Array[String],
    startOffsets: Array[Int],
    endOffsets: Array[Int]
  ): Sentence = new Sentence(words, startOffsets, endOffsets)
  def apply(
    words: Array[String],
    startOffsets: Array[Int],
    endOffsets: Array[Int],
    tags: Option[Array[String]],
    lemmas: Option[Array[String]],
    entities: Option[Array[String]],
    norms: Option[Array[String]],
    chunks: Option[Array[String]],
    tree: Option[Tree],
    deps: GraphMap
  ): Sentence = {
    val s = Sentence(words, startOffsets, endOffsets)
    // update annotations
    s.tags = tags
    s.lemmas = lemmas
    s.entities = entities
    s.norms = norms
    s.chunks = chunks
    s.syntacticTree = tree
    s.dependenciesByType = deps
    s
  }
}