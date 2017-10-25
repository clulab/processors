package org.clulab.sequences

/**
  * Generates all accepted lexical variations for an entity
  * User: mihais
  * Date: 10/3/17
  */
trait LexicalVariations extends Serializable {
  def lexicalVariations(tokens: Array[String]): Seq[Array[String]]
}

class NoLexicalVariations extends LexicalVariations {
  override def lexicalVariations(tokens: Array[String]): Seq[Array[String]] = Seq.empty
}