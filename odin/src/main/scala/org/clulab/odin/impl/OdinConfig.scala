package org.clulab.odin.impl

import org.clulab.struct.GraphMap


/**
  * Encapsulates the properties needed for interpreting a rule (resources, graph type, taxonomy, etc.). <br>
  * New properties utilized by all rules should be added to the OdinConfig.
  */
case class OdinConfig(
  taxonomy: Option[Taxonomy] = None,
  variables: Map[String, String] = Map.empty[String, String],
  resources: OdinResourceManager = OdinResourceManager(Map.empty),
  graph: String = OdinConfig.DEFAULT_GRAPH
)

object OdinConfig {

  val DEFAULT_GRAPH = GraphMap.STANFORD_COLLAPSED

  def empty: OdinConfig = OdinConfig()

  // add newly registered graph types here
  var VALID_GRAPHS = Set[String](GraphMap.STANFORD_COLLAPSED, GraphMap.STANFORD_BASIC, GraphMap.SEMANTIC_ROLES)
}