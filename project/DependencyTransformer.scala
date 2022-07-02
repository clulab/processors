package org.clulab.sbt

import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.transform.RewriteRule

case class DependencyId(groupId: String, artifactId: String)

abstract class DependencyTransformer extends RewriteRule {

  override def transform(node: Node): NodeSeq = {
    val name = node.nameToString(new StringBuilder()).toString()

    name match {
      case "dependency" =>
        val groupId = (node \ "groupId").text.trim
        val artifactId = (node \ "artifactId").text.trim

        transform(node, DependencyId(groupId, artifactId))
      case _ => node
    }
  }

  def transform(node: Node, dependencyId: DependencyId): NodeSeq
}

class DependencyFilter(filter: DependencyId => Boolean) extends DependencyTransformer {

  def transform(node: Node, dependencyId: DependencyId): NodeSeq =
      if (filter(dependencyId)) node
      else Nil
}

object DependencyFilter {

  def apply(filter: DependencyId => Boolean): DependencyFilter = new DependencyFilter(filter)
}
