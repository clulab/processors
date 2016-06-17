package org.clulab.odin

/** Represents the current state of the ExtractorEngine.
  * Contains all the mentions found in previous iterations.
  */
class State(val lookUpTable: MentionLUT) {

  import State._

  /** Makes an empty state */
  def this() = this(Map.empty)

  /** Merges the current state with the provided mentions and returns a new State object. */
  def updated(mentions: Seq[Mention]): State =
    new State(mergeLuts(lookUpTable, mkLut(mentions)))

  /** Returns all mentions contained in the state for which keep == true */
  def allMentions: Vector[Mention] =
    lookUpTable.values.toStream.flatten.distinct.filter(_.keep).toVector

  /** Checks if a mention is already contained in the state */
  def contains(m: Mention): Boolean =
    mentionsFor(m.sentence, m.start) contains m

  /** Returns all mentions for a given sentence and token */
  def mentionsFor(sent: Int, tok: Int): Seq[Mention] =
    lookUpTable.getOrElse((sent, tok), Nil)

  /** Returns all mentions for a given sentence */
  def mentionsFor(sent: Int): Seq[Mention] = {
    val mentions = for {
      (s, t) <- lookUpTable.keys
      if s == sent
      m <- lookUpTable((s, t))
    } yield m
    mentions.toList.distinct
  }

  /** Returns all mentions for a given sentence and token that match a given label */
  def mentionsFor(sent: Int, tok: Int, label: String): Seq[Mention] =
    mentionsFor(sent, tok).filter(_ matches label)

  /** Returns all mentions for a given sentence and token that match any of the given labels */
  def mentionsFor(sent: Int, tok: Int, labels: Seq[String]): Seq[Mention] =
    labels.flatMap(l => mentionsFor(sent, tok, l)).distinct

  /** Returns all mentions for a given sentence and any of the given tokens */
  def mentionsFor(sent: Int, toks: Seq[Int]): Seq[Mention] =
    toks.flatMap(t => mentionsFor(sent, t)).distinct

  /** Returns all mentions for a given sentence and any of the given tokens
    * that match a given label
    */
  def mentionsFor(sent: Int, toks: Seq[Int], label: String): Seq[Mention] =
    toks.flatMap(t => mentionsFor(sent, t, label)).distinct

  /** Returns all mentions for a given sentence and any of the given tokens
    * that match any of the given labels
    */
  def mentionsFor(sent: Int, toks: Seq[Int], labels: Seq[String]): Seq[Mention] =
    toks.flatMap(t => mentionsFor(sent, t, labels)).distinct

}

object State {
  def apply(mentions: Seq[Mention]): State = new State(mkLut(mentions))

  def mkLut(mentions: Seq[Mention]): MentionLUT = {
    val pairs = for {
      m <- mentions
      i <- m.tokenInterval
      key = (m.sentence, i)
    } yield (key, m)
    pairs groupBy (_._1) transform ((k, v) => v.map(_._2))
  }

  def mergeLuts(lhs: MentionLUT, rhs: MentionLUT): MentionLUT = {
    val merged = for {
      key <- lhs.keySet ++ rhs.keySet
      mentions = lhs.getOrElse(key, Nil) ++ rhs.getOrElse(key, Nil)
    } yield (key -> mentions.distinct)
    merged.toMap
  }
}
