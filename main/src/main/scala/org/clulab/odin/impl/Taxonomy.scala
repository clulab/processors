package org.clulab.odin.impl

import java.util.{ Collection, Map => JMap }
import scala.collection.JavaConverters._

class Taxonomy(parents: Map[String, String]) {

  import Taxonomy.ROOT

  /** returns true if term is defined in taxonomy, false otherwise */
  def contains(term: String): Boolean =
    parents contains term

  /** returns true if hypernym is a superclass of hyponym, or if hypernym == hyponym */
  def isa(hyponym: String, hypernym: String): Boolean =
    lazyHypernymsFor(hyponym) contains hypernym

  // builds a sequence of hypernyms lazily
  def lazyHypernymsFor(term: String): Stream[String] = term match {
    case ROOT => Stream.empty
    case node if this.contains(node) => node #:: lazyHypernymsFor(parents(node))
    case node => throw new OdinException(s"term '$node' not in taxonomy")
  }

  /** returns the term and all its hypernyms */
  def hypernymsFor(term: String): List[String] =
    lazyHypernymsFor(term).toList

  /** returns the term and all its hyponyms */
  def hyponymsFor(term: String): List[String] = {
    @annotation.tailrec
    def collect(remaining: List[String], results: List[String]): List[String] = remaining match {
      case Nil => results
      case head :: tail if this.contains(head) =>
        val children = for ((child, parent) <- parents if parent == head) yield child
        collect(tail ++ children, head :: results)
      case head :: tail => throw new OdinException(s"term '$head' not in taxonomy")
    }
    collect(List(term), Nil)
  }

}

object Taxonomy {

  val ROOT = "**ROOT**"

  def apply(forest: Collection[Any]): Taxonomy = new Taxonomy(mkParents(forest))

  def mkParents(nodes: Collection[Any]): Map[String, String] =
    mkParents(nodes.asScala.toSeq, ROOT, Map.empty)

  def mkParents(
      nodes: Seq[Any],
      parent: String,
      table: Map[String,String]
  ): Map[String, String] = nodes match {
    case Nil =>
      // we are done parsing, return the parents table
      table
    case (term: String) +: tail =>
      if (table contains term) {
        throw new OdinCompileException(s"duplicated taxonomy term '$term'")
      }
      // add term to parents table and continue parsing siblings
      mkParents(tail, parent, table.updated(term, parent))
    case head +: tail =>
      // get next node as a scala map
      val map = head.asInstanceOf[JMap[String, Collection[Any]]].asScala
      if (map.keys.size != 1) {
        val labels = map.keys.mkString(", ")
        throw new OdinCompileException(s"taxonomy tree node with multiple labels: $labels")
      }
      val term = map.keys.head
      if (table contains term) {
        throw new OdinCompileException(s"duplicated taxonomy term '$term'")
      }
      Option(map(term)) match {
        case None =>
          val msg = s"taxonomy term '$term' has no children (looks like an extra ':')"
          throw new OdinCompileException(msg)
        case Some(children) =>
          // 1. add term to parents table
          // 2. parse children
          // 3. parse siblings
          mkParents(tail, parent, mkParents(children.asScala.toSeq, term, table.updated(term, parent)))
      }
  }

}
