package org.clulab.sbt

import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.transform.RewriteRule

abstract class DependencyTransformer extends RewriteRule {
  println("DependencyTransformer is being created!")

  override def transform(node: Node): NodeSeq = {
    val name = node.nameToString(new StringBuilder()).toString()

    name match {
      case "dependency" =>
        val groupId = (node \ "groupId").text.trim
        val artifactId = (node \ "artifactId").text.trim

        println(s"Transforming $groupId % artifactId")
        transform(node, groupId, artifactId)
      case _ => node
    }
  }

  def transform(node: Node, groupId: String, artifactId: String): NodeSeq
}

abstract class DependencyFilter extends DependencyTransformer {

  def transform(node: Node, groupId: String, artifactId: String): NodeSeq =
      if (filter(groupId, artifactId)) node
      else Nil

  def filter(groupId: String, artifactId: String): Boolean
}

class ExclusiveDependencyFilter(groupId: String, artifactId: String) extends DependencyFilter {

  def filter(groupId: String, artifactId: String): Boolean = {
      // These will be excluded.
      !(this.groupId == groupId && this.artifactId == artifactId)
  }
}

object ExclusiveDependencyFilter {

  def apply(groupId: String, artifactId: String): ExclusiveDependencyFilter =
      new ExclusiveDependencyFilter(groupId, artifactId)
}
