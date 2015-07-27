package edu.arizona.sista.odin.impl

import java.util.{ Collection, Map => JMap }
import scala.collection.JavaConverters._
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

class Taxonomy(parents: Map[String, String]) {

  import Taxonomy.ROOT

  /** returns true if term is defined in taxonomy, false otherwise */
  def contains(term: String): Boolean = parents contains term

  /** returns true if hypernym is a superclass of hyponym, or if hypernym == hyponym */
  def isa(hyponym: String, hypernym: String): Boolean = labelsFor(hyponym) contains hypernym

  /** returns the most recent common ancestor for term1 and term2 if there is one */
  def commonAncestor(term1: String, term2: String): Option[String] = {
    var ancestor: Option[String] = None
    for ((l1, l2) <- labelsFor(term1) zip labelsFor(term2)) {
      if (l1 == l2) ancestor = Some(l1)
      else return ancestor
    }
    ancestor
  }

  /** returns the term and all its hypernyms */
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
      // we are done parsing, return the parents table
      parents
    case (term: String) +: tail =>
      if (parents contains term) {
        throw new OdinCompileException(s"duplicated taxonomy term '$term'")
      }
      // add term to parents table and continue parsing siblings
      mkParents(tail, parent, parents.updated(term, parent))
    case head +: tail =>
      // get next node as a scala map
      val map = head.asInstanceOf[JMap[String, Collection[Any]]].asScala
      if (map.keys.size != 1) {
        val labels = map.keys.mkString(", ")
        throw new OdinCompileException(s"taxonomy tree node with multiple labels: $labels")
      }
      val term = map.keys.head
      if (parents contains term) {
        throw new OdinCompileException(s"duplicated taxonomy term '$term'")
      }
      Option(map(term)) match {
        case None =>
          throw new OdinCompileException(s"taxonomy term '$term' has no children (looks like an extra ':')")
        case Some(children) =>
          // 1. add term to parents table
          // 2. parse children
          // 3. parse siblings
          mkParents(tail, parent, mkParents(children.asScala.toSeq, term, parents.updated(term, parent)))
      }
  }

}
