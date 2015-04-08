package edu.arizona.sista.odin

import scala.util.hashing.MurmurHash3._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin.impl.StringMatcher

trait Mention extends Equals {
  /** A sequence of labels for this mention.
    * The first label in the sequence is considered the default.
    */
  def labels: Seq[String]

  /** The interval of token indicess that form this mention. */
  def tokenInterval: Interval

  /** The index of the sentence where this mention occurs. */
  def sentence: Int

  /** The document where this mention occurs. */
  def document: Document

  /** Should we report this mention at the end? */
  def keep: Boolean

  /** A map from argument name to a sequence of mentions.
    *
    * The value of the map is a sequence because there are events
    * that can have several arguments with the same name.
    * For example, in the biodomain, Binding may have several themes.
    */
  val arguments: Map[String, Seq[Mention]]

  /** points to an Xref object that represents an entry in an external database */
  var xref: Option[Xref] = None
  def isGrounded: Boolean = xref.isDefined
  def ground(namespace: String, id: String) = xref = Some(Xref(namespace, id))

  /** default label */
  def label: String = labels.head

  /** name of matching rule */
  def foundBy: String

  /** index of the first token in the mention */
  def start: Int = tokenInterval.start

  /** one after the last token in the mention */
  def end: Int = tokenInterval.end

  /** character offset of the mention beginning */
  def startOffset: Int = document.sentences(sentence).startOffsets(start)

  /** character offset of the mention end */
  def endOffset: Int = document.sentences(sentence).endOffsets(end - 1)

  /** returns true if the string matches any of the mention labels */
  def matches(label: String): Boolean = labels contains label

  /** returns true if the StringMatcher matches any of the mention labels */
  def matches(matcher: StringMatcher): Boolean = labels exists matcher.matches

  /** returns all tokens in mention */
  def words: Seq[String] = document.sentences(sentence).words.slice(start, end)

  /** returns a string that contains the mention */
  // FIXME This string doesn't match the original text. We can do better than this.
  def text: String = words.mkString(" ")

  override def canEqual(a: Any) = a.isInstanceOf[Mention]

  override def equals(that: Any): Boolean = that match {
    case that: Mention => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = {
    val h0 = symmetricSeed
    val h1 = mix(h0, labels.hashCode)
    val h2 = mix(h1, tokenInterval.hashCode)
    val h3 = mix(h2, sentence.hashCode)
    val h4 = mix(h3, document.hashCode)
    val h5 = mixLast(h4, arguments.hashCode)
    finalizeHash(h5, 5)
  }
}

class TextBoundMention(
    val labels: Seq[String],
    val tokenInterval: Interval,
    val sentence: Int,
    val document: Document,
    val keep: Boolean,
    val foundBy: String
) extends Mention {

  def this(
    label: String,
    tokenInterval: Interval,
    sentence: Int,
    document: Document,
    keep: Boolean,
    foundBy: String
  ) = this(Seq(label), tokenInterval, sentence, document, keep, foundBy)

  // TextBoundMentions don't have arguments
  val arguments: Map[String, Seq[Mention]] = Map.empty
}

class EventMention(
    val labels: Seq[String],
    val trigger: TextBoundMention,
    val arguments: Map[String, Seq[Mention]],
    val sentence: Int,
    val document: Document,
    val keep: Boolean,
    val foundBy: String
) extends Mention {

  def this(
    label: String,
    trigger: TextBoundMention,
    arguments: Map[String, Seq[Mention]],
    sentence: Int,
    document: Document,
    keep: Boolean,
    foundBy: String
  ) = this(Seq(label), trigger, arguments, sentence, document, keep, foundBy)

  // token interval that contains trigger and all matched arguments
  override def tokenInterval: Interval = {
    val allStarts = trigger.start +: arguments.values.flatMap(_.map(_.start)).toSeq
    val allEnds = trigger.end +: arguments.values.flatMap(_.map(_.end)).toSeq
    Interval(allStarts.min, allEnds.max)
  }
}

class RelationMention(
    val labels: Seq[String],
    val arguments: Map[String, Seq[Mention]],
    val sentence: Int,
    val document: Document,
    val keep: Boolean,
    val foundBy: String
) extends Mention {

  require(arguments.values.flatten.nonEmpty, "RelationMentions need arguments")

  def this(
    label: String,
    arguments: Map[String, Seq[Mention]],
    sentence: Int,
    document: Document,
    keep: Boolean,
    foundBy: String
  ) = this(Seq(label), arguments, sentence, document, keep, foundBy)

  // token interval that contains all matched arguments
  override def tokenInterval: Interval = {
    val allStarts = arguments.values.flatMap(_.map(_.start)).toSeq
    val allEnds = arguments.values.flatMap(_.map(_.end)).toSeq
    Interval(allStarts.min, allEnds.max)
  }
}
