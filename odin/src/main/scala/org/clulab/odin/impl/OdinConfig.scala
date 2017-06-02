package org.clulab.odin.impl


case class OdinConfig(
  taxonomy: Option[Taxonomy] = None,
  variables: Map[String, String] = Map.empty[String, String],
  resources: OdinResourceManager
) {
  def copy(
    taxonomy: Option[Taxonomy] = this.taxonomy,
    variables: Map[String, String] = this.variables,
    resources: OdinResourceManager = this.resources
  ): OdinConfig = OdinConfig(taxonomy, variables, resources)
}
