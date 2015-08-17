package edu.arizona.sista.odin

import scala.util.hashing.MurmurHash3._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin.impl.StringMatcher

trait Mention extends Equals with Ordered[Mention] {
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

  /** default label */
  def label: String = labels.head

  /** name of matching rule */
  def foundBy: String

  /** index of the first token in the mention */
  def start: Int = tokenInterval.start

  /** one after the last token in the mention */
  def end: Int = tokenInterval.end

  def sentenceObj = document.sentences(sentence)

  /** character offset of the mention beginning */
  def startOffset: Int = sentenceObj.startOffsets(start)

  /** character offset of the mention end */
  def endOffset: Int = sentenceObj.endOffsets(end - 1)

  /** returns true if this is a valid mention */
  def isValid: Boolean = true

  /** returns true if the string matches any of the mention labels */
  def matches(label: String): Boolean = labels contains label

  /** returns true if the StringMatcher matches any of the mention labels */
  def matches(matcher: StringMatcher): Boolean = labels exists matcher.matches

  /** returns all tokens in mention */
  def words: Seq[String] = sentenceObj.words.slice(start, end)

  def tags: Option[Seq[String]] = sentenceObj.tags.map(_.slice(start, end))

  def lemmas: Option[Seq[String]] = sentenceObj.lemmas.map(_.slice(start, end))

  def entities: Option[Seq[String]] = sentenceObj.entities.map(_.slice(start, end))

  def chunks: Option[Seq[String]] = sentenceObj.chunks.map(_.slice(start, end))

  /** returns a string that contains the mention */
  def text: String = document.text match {
    case Some(txt) => txt.slice(startOffset, endOffset)
    case None =>
      // try to reconstruct the sentence using the character offsets
      val bits = words.head +: tokenInterval.drop(1).map { i =>
        val spaces = " " * (sentenceObj.startOffsets(i) - sentenceObj.endOffsets(i - 1))
        val word = sentenceObj.words(i)
        spaces + word
      }
      bits.mkString
  }

  override def canEqual(a: Any) = a.isInstanceOf[Mention]

  override def equals(that: Any): Boolean = that match {
    case that: Mention => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  def compare(that: Mention): Int = {
    require(this.document == that.document)
    if (this.sentence < that.sentence) -1
    else if (this.sentence > that.sentence) 1
    else this.tokenInterval compare that.tokenInterval
  }

  def precedes(that: Mention): Boolean = {
    this.compare(that) match {
      case c if c < 0 => true
      case _ => false
    }
  }

  override def hashCode: Int = {
    val h0 = stringHash("edu.arizona.sista.odin.Mention")
    val h1 = mix(h0, labels.hashCode)
    val h2 = mix(h1, tokenInterval.hashCode)
    val h3 = mix(h2, sentence.hashCode)
    val h4 = mix(h3, document.hashCode)
    val h5 = mixLast(h4, argumentsHashCode)
    finalizeHash(h5, 5)
  }

  private def argumentsHashCode: Int = {
    val h0 = stringHash("Mention.arguments")
    val hs = arguments map {
      case (name, args) => mix(stringHash(name), unorderedHash(args))
    }
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, arguments.size)
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

  // Create a new TextBoundMention by changing only the sequence of labels
  def copy(newLabels: Seq[String]): TextBoundMention = {
    new TextBoundMention(
      newLabels,
      tokenInterval,
      sentence,
      document,
      keep,
      foundBy
    )
  }

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

  require(arguments.values.flatten.nonEmpty || labels.contains("Unresolved"), "EventMentions need arguments")

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

  override def isValid: Boolean = {
    // get token interval for trigger
    val trig = trigger.tokenInterval
    // get token intervals for arguments
    val args = arguments.values.flatten.map(_.tokenInterval)
    // the event is invalid if the trigger is contained by one of its arguments
    !args.exists(_.contains(trig))
  }

  // trigger should be part of the hashCode too
  override def hashCode: Int = {
    val h0 = stringHash("edu.arizona.sista.odin.EventMention")
    val h1 = mix(h0, super.hashCode)
    val h2 = mixLast(h1, trigger.hashCode)
    finalizeHash(h2, 2)
  }

  // Create a new EventMention by changing only the sequence of labels
  def copy(newLabels: Seq[String]): EventMention = {
    new EventMention(
      newLabels,
      trigger,
      arguments,
      sentence,
      document,
      keep,
      foundBy
    )
  }

  // Convert an EventMention to a RelationMention by deleting the trigger
  def toRelationMention: RelationMention = {
    new RelationMention(
      labels,
      arguments,
      sentence,
      document,
      keep,
      s"$foundBy + toRelationMention"
    )
  }

  // Create a new EventMention by removing a single argument
  def -(argName:String): EventMention = {
    new EventMention(
      labels,
      trigger,
      arguments - argName,
      sentence,
      document,
      keep,
      foundBy
    )
  }

  // Create a new EventMention by removing a sequence of arguments
  def -(argNames:Seq[String]): EventMention = {
    new EventMention(
      labels,
      trigger,
      // Remove each key
      argNames.foldRight(arguments){ (arg, argMap) => argMap - arg },
      sentence,
      document,
      keep,
      foundBy
    )
  }

  // Create a new EventMention by adding a key, value pair to the arguments map
  def +(argName:String, mentions:Seq[Mention]): EventMention = {
    new EventMention(
      labels,
      trigger,
      arguments + (argName -> mentions),
      sentence,
      document,
      keep,
      foundBy
    )
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
    val allStarts = arguments.values.flatMap(_.map(_.start))
    val allEnds = arguments.values.flatMap(_.map(_.end))
    Interval(allStarts.min, allEnds.max)
  }

  // Create a new RelationMention by changing only the sequence of labels
  def copy(newLabels: Seq[String]): RelationMention = {
    new RelationMention(
      newLabels,
      arguments,
      sentence,
      document,
      keep,
      foundBy
    )
  }

  // Convert a RelationMention to an EventMention by specifying a trigger
  def toEventMention(trigger: TextBoundMention): RelationMention = {
    new RelationMention(
      labels,
      arguments,
      sentence,
      document,
      keep,
      s"$foundBy + toEventMention"
    )
  }


  // Create a new RelationMention by removing a single argument
  def -(argName:String): RelationMention = {
    new RelationMention(
      labels,
      arguments - argName,
      sentence,
      document,
      keep,
      foundBy
    )
  }

  // Create a new RelationMention by removing a sequence of arguments
  def -(argNames:Seq[String]): RelationMention = {
    new RelationMention(
      labels,
      // Remove each key
      argNames.foldRight(arguments){ (arg, argMap) => argMap - arg },
      sentence,
      document,
      keep,
      foundBy
    )
  }

}
