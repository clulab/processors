package edu.arizona.sista.odin.impl

import edu.arizona.sista.processors.Document

trait Values {
  def values(strings: Option[Array[String]], msg: String): Array[String] =
    strings match {
      case None => sys.error(msg)
      case Some(strings) => strings
    }

  def word(tok: Int, sent: Int, doc: Document): String =
    doc.sentences(sent).words(tok)

  def words(tokens: Seq[Int], sent: Int, doc: Document): Seq[(Int, String)] = {
    val strings = doc.sentences(sent).words
    tokens map (i => (i, strings(i)))
  }

  def lemma(tok: Int, sent: Int, doc: Document): String = {
    val lemmas = values(doc.sentences(sent).lemmas, "sentence has no lemmas")
    lemmas(tok)
  }

  def lemmas(tokens: Seq[Int], sent: Int, doc: Document): Seq[(Int, String)] = {
    val strings = values(doc.sentences(sent).lemmas, "sentence has no lemmas")
    tokens map (i => (i, strings(i)))
  }

  def tag(tok: Int, sent: Int, doc: Document): String = {
    val tags = values(doc.sentences(sent).tags, "sentence has no tags")
    tags(tok)
  }

  def tags(tokens: Seq[Int], sent: Int, doc: Document): Seq[(Int, String)] = {
    val strings = values(doc.sentences(sent).tags, "sentence has no tags")
    tokens map (i => (i, strings(i)))
  }

  def entity(tok: Int, sent: Int, doc: Document): String = {
    val entities = values(doc.sentences(sent).entities, "sentence has no entities")
    entities(tok)
  }

  def entities(tokens: Seq[Int], sent: Int, doc: Document): Seq[(Int, String)] = {
    val strings = values(doc.sentences(sent).entities, "sentence has no entities")
    tokens map (i => (i, strings(i)))
  }

  def chunk(tok: Int, sent: Int, doc: Document): String = {
    val chunks = values(doc.sentences(sent).chunks, "sentence has no chunks")
    chunks(tok)
  }

  def chunks(tokens: Seq[Int], sent: Int, doc: Document): Seq[(Int, String)] = {
    val strings = values(doc.sentences(sent).chunks, "sentence has no chunks")
    tokens map (i => (i, strings(i)))
  }
}
