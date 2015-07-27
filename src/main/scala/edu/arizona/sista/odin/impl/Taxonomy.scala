package edu.arizona.sista.odin.impl

import java.util.{ Collection, Map => JMap }
import scala.collection.JavaConverters._
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

class Taxonomy(parents: Map[String, String]) {

  import Taxonomy.ROOT

  private val vocabulary: Set[String] = parents.keySet

  def contains(term: String): Boolean = vocabulary contains term

  def isa(hyponym: String, hypernym: String): Boolean = labelsFor(hyponym) contains hypernym

  def commonAncestor(term1: String, term2: String): Option[String] = {
    var ancestor: Option[String] = None
    for ((l1, l2) <- labelsFor(term1) zip labelsFor(term2)) {
      if (l1 == l2) ancestor = Some(l1)
      else return ancestor
    }
    ancestor
  }

  def labelsFor(term: String): List[String] = {
    @annotation.tailrec
    def collect(nodes: List[String]): List[String] = parents(nodes.head) match {
      case ROOT => nodes
      case p => collect(p :: nodes)
    }
    collect(List(term))
  }

}

object Taxonomy {

  val ROOT = "**ROOT**"

  def apply(input: String): Taxonomy = {
    val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
    val roots = yaml.load(input).asInstanceOf[Collection[Any]]
    new Taxonomy(mkParents(roots))
  }

  def mkParents(nodes: Collection[Any]): Map[String, String] =
    mkParents(nodes.asScala.toSeq, ROOT, Map.empty)

  def mkParents(
      nodes: Seq[Any],
      parent: String,
      parents: Map[String,String]
  ): Map[String, String] = nodes match {
    case Nil =>
      parents
    case (head: String) +: tail =>
      if (parents.keySet contains head)
        throw new OdinCompileException(s"duplicated taxonomy term '$head'")
      mkParents(tail, parent, parents.updated(head, parent))
    case head +: tail =>
      val m = head.asInstanceOf[JMap[String, Collection[Any]]].asScala
      if (m.keys.size != 1)
        throw new OdinCompileException("malformed taxonomy tree")
      val k = m.keys.head
      if (parents.keySet contains k)
        throw new OdinCompileException(s"duplicated taxonomy term '$k'")
      val v = m(k).asScala.toSeq
      mkParents(tail, parent, mkParents(v, k, parents.updated(k, parent)))
  }

}
