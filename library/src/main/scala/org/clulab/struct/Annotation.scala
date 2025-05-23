package org.clulab.struct

import org.clulab.struct.GraphMap.GraphMap

// These are by the word ones and then there are relationships between words.
// So parse, might not be a thing that is per word.
//case class WordParse(tag: String, lemma: String, entity: String, norm: String, chunk: String)

//case class SentenceParse(tags: Array[String], syntacticTree, graphs, relations)

case class Annotation(
  tags: Option[Array[String]] = None,
  /** Lemmas */
  lemmas: Option[Array[String]] = None,
  /** NE labels */
  entities: Option[Array[String]] = None,
  /** Normalized values of named/numeric entities, such as dates */
  norms: Option[Array[String]] = None,
  /** Shallow parsing labels */
  chunks: Option[Array[String]] = None,
  /** Constituent tree of this sentence; includes head words */
  syntacticTree: Option[Tree] = None,
  /** DAG of syntactic and semantic dependencies; word offsets start at 0 */
  graphs: GraphMap = GraphMap(),
  /** Relation triples from OpenIE */
  relations:Option[Array[RelationTriple]] = None
) {

  def reverse: Annotation = {
    Annotation(
      tags = tags.map(_.reverse),
      lemmas = lemmas.map(_.reverse),
      entities = entities.map(_.reverse),
      norms = norms.map(_.reverse),
      chunks = chunks.map(_.reverse)
      // TODO: reverse syntacticTree, graphs, and relations!
    )
  }
}
