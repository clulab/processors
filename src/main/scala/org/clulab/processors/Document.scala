package org.clulab.processors

import org.clulab.discourse.rstparser.DiscourseTree
import org.clulab.struct.CorefChains


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