package org.clulab.processors

import org.clulab.discourse.rstparser.DiscourseTree
import org.clulab.struct.CorefChains


/**
  * Stores all annotations for one document
  */
class Document(val sentences: Array[Sentence]) extends Serializable {

  var id: Option[String] = None
  // FIXME: are coreferenceChains needed? Seems like a CoreNLP-specific thing...
  var coreferenceChains: Option[CorefChains] = None
  var discourseTree: Option[DiscourseTree] = None
  var text: Option[String] = None

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

object Document {

  def apply(sentences: Array[Sentence]): Document = new Document(sentences)
  def apply(id: Option[String], sentences: Array[Sentence], coref: Option[CorefChains], dtree: Option[DiscourseTree], text: Option[String]): Document = {
    val d = Document(sentences)
    d.id = id
    d.coreferenceChains = coref
    d.discourseTree = dtree
    d.text = text
    d
  }
}
