package org.clulab.odin.impl

import org.clulab.struct.GraphMap


case class OdinConfig(
  taxonomy: Option[Taxonomy] = None,
  variables: Map[String, String] = Map.empty[String, String],
  resources: OdinResourceManager = OdinResourceManager(Map.empty),
  graph: String = OdinConfig.DEFAULT_GRAPH
) {
  def copy(
    taxonomy: Option[Taxonomy] = this.taxonomy,
    variables: Map[String, String] = this.variables,
    resources: OdinResourceManager = this.resources,
    graph: String = this.graph
  ): OdinConfig = OdinConfig(taxonomy, variables, resources, graph)
}


object OdinConfig {

  val DEFAULT_GRAPH = GraphMap.STANFORD_COLLAPSED

  def empty: OdinConfig = OdinConfig()
}