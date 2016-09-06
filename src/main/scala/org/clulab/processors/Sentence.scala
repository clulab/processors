package org.clulab.processors

import org.clulab.struct.{DependencyMap, DirectedGraph, Tree}
import org.clulab.struct.GraphMap._

import scala.collection.mutable


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