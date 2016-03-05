package edu.arizona.sista.odin

import scala.util.matching.Regex
import scala.util.hashing.MurmurHash3._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.utils.DependencyUtils
import edu.arizona.sista.odin.impl.StringMatcher
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native._

@SerialVersionUID(1L)
trait Mention extends Equals with Ordered[Mention] with Serializable {

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

  val paths: Map[String, Map[Mention, SynPath]]

  def getPath(argRole: String, mention: Mention): SynPath = paths(argRole)(mention)

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

  /** returns true if the regex matches any of the mention labels */
  def matches(regex: Regex): Boolean = labels.exists(l => regex.findFirstIn(l).nonEmpty)

  /** returns true if the StringMatcher matches any of the mention labels */
  def matches(matcher: StringMatcher): Boolean = labels exists matcher.matches

  /** returns all tokens in mention */
  def words: Seq[String] = sentenceObj.words.slice(start, end)

  /** returns all tags in mention */
  def tags: Option[Seq[String]] = sentenceObj.tags.map(_.slice(start, end))

  /** returns all lemmas in mention */
  def lemmas: Option[Seq[String]] = sentenceObj.lemmas.map(_.slice(start, end))

  /** returns all entities in mention */
  def entities: Option[Seq[String]] = sentenceObj.entities.map(_.slice(start, end))

  /** returns all chunks in mention */
  def chunks: Option[Seq[String]] = sentenceObj.chunks.map(_.slice(start, end))

  /** Return all syntactic heads, where a head is a token in `mention`
    * whose parent isn't contained in `mention`.
    */
  def synHeads: Seq[Int] = {
    if (tokenInterval.isEmpty) {
      Nil
    } else if (tokenInterval.size == 1) {
      // we don't need dependencies, a single token is its own head
      tokenInterval
    } else {
      DependencyUtils.findHeadsStrict(tokenInterval, sentenceObj)
    }
  }

  /** Return the syntactic head of `mention`  */
  def synHead: Option[Int] = synHeads.lastOption

  /** returns head token */
  def synHeadWord: Option[String] = synHead.map(i => sentenceObj.words(i))

  /** returns head pos tag */
  def synHeadTag: Option[String] = synHead.flatMap(i => sentenceObj.tags.map(_(i)))

  /** returns head lemma */
  def synHeadLemma: Option[String] = synHead.flatMap(i => sentenceObj.lemmas.map(_(i)))

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

  def jsonAST: JValue

  def json(pretty: Boolean = false): String =
    if (pretty) prettyJson(renderJValue(jsonAST))
    else compactJson(renderJValue(jsonAST))

  override def canEqual(a: Any) = a.isInstanceOf[Mention]

  override def equals(that: Any): Boolean = that match {
    case that: Mention => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  def compare(that: Mention): Int = {
    require(this.document == that.document,
      "can't compare mentions if they belong to different documents")
    if (this.sentence < that.sentence) -1
    else if (this.sentence > that.sentence) 1
    else this.tokenInterval compare that.tokenInterval
  }

  def precedes(that: Mention): Boolean = this.compare(that) < 0

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
  val paths: Map[String, Map[Mention, SynPath]] = Map.empty

  def jsonAST: JValue = {
    ("type" -> "TextBound") ~
    ("tokenInterval" -> List(start, end)) ~
    ("characterOffsets" -> List(startOffset, endOffset)) ~
    ("labels" -> labels) ~
    ("sentence" -> sentence) ~
    ("foundBy" -> foundBy)
  }

  // Copy constructor for TextBoundMention
  def copy(
      labels: Seq[String] = this.labels,
      tokenInterval: Interval = this.tokenInterval,
      sentence: Int = this.sentence,
      document: Document = this.document,
      keep: Boolean = this.keep,
      foundBy: String = this.foundBy
  ): TextBoundMention = new TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy)

}

// NOTE that event mentions *may* have no arguments
// this is allowed because it is useful for coreference
class EventMention(
    val labels: Seq[String],
    val tokenInterval: Interval,
    val trigger: TextBoundMention,
    val arguments: Map[String, Seq[Mention]],
    val paths: Map[String, Map[Mention, SynPath]],
    val sentence: Int,
    val document: Document,
    val keep: Boolean,
    val foundBy: String
) extends Mention {

  def this(
      label: String,
      trigger: TextBoundMention,
      arguments: Map[String, Seq[Mention]],
      paths: Map[String, Map[Mention, SynPath]],
      sentence: Int,
      document: Document,
      keep: Boolean,
      foundBy: String
  ) = this(Seq(label), mkTokenInterval(trigger, arguments), trigger, arguments, paths, sentence, document, keep, foundBy)

  def this(
      label: String,
      trigger: TextBoundMention,
      arguments: Map[String, Seq[Mention]],
      sentence: Int,
      document: Document,
      keep: Boolean,
      foundBy: String
  ) = this(Seq(label), mkTokenInterval(trigger, arguments), trigger, arguments, Map.empty[String, Map[Mention, SynPath]], sentence, document, keep, foundBy)

  def this(
      labels: Seq[String],
      trigger: TextBoundMention,
      arguments: Map[String, Seq[Mention]],
      sentence: Int,
      document: Document,
      keep: Boolean,
      foundBy: String
  ) = this(labels, mkTokenInterval(trigger, arguments), trigger, arguments, Map.empty[String, Map[Mention, SynPath]], sentence, document, keep, foundBy)

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

  def jsonAST: JValue = {
    val args = arguments.map {
      case (name, mentions) => (name -> JArray(mentions.map(_.jsonAST).toList))
    }
    ("type" -> "Event") ~
    ("labels" -> labels) ~
    ("sentence" -> sentence) ~
    ("foundBy" -> foundBy) ~
    ("trigger" -> trigger.jsonAST) ~
    ("arguments" -> JObject(args.toList))
  }

  // Copy constructor for EventMention
  def copy(
      labels: Seq[String] = this.labels,
      tokenInterval: Interval = this.tokenInterval,
      trigger: TextBoundMention = this.trigger,
      arguments: Map[String, Seq[Mention]] = this.arguments,
      paths: Map[String, Map[Mention, SynPath]] = this.paths,
      sentence: Int = this.sentence,
      document: Document = this.document,
      keep: Boolean = this.keep,
      foundBy: String = this.foundBy
  ): EventMention = new EventMention(labels, tokenInterval, trigger, arguments, paths, sentence, document, keep, foundBy)

  // Convert an EventMention to a RelationMention by deleting the trigger
  def toRelationMention: RelationMention = {
    new RelationMention(
      this.labels,
      this.tokenInterval,
      this.arguments,
      this.paths,
      this.sentence,
      this.document,
      this.keep,
      s"${this.foundBy} + toRelationMention"
    )

  }

  // scatters the args named `argName` into N mentions each with `size` args named `argName`
  // all combinations of args are produced
  def scatter(argName: String, size: Int): Seq[EventMention] =
    arguments
      .getOrElse(argName, Nil)
      .combinations(size)
      .map(args => this + (argName -> args))
      .toList

  // Create a new EventMention by removing a single argument
  def -(argName: String): EventMention =
    copy(arguments = this.arguments - argName)

  // Create a new EventMention by removing a sequence of arguments
  def --(argNames: Seq[String]): EventMention =
    copy(arguments = this.arguments -- argNames)

  // Create a new EventMention by adding a key, value pair to the arguments map
  def +(arg: (String, Seq[Mention])): EventMention =
    copy(arguments = this.arguments + arg)

}

class RelationMention(
    val labels: Seq[String],
    val tokenInterval: Interval,
    val arguments: Map[String, Seq[Mention]],
    val paths: Map[String, Map[Mention, SynPath]],
    val sentence: Int,
    val document: Document,
    val keep: Boolean,
    val foundBy: String
) extends Mention {

  require(arguments.values.flatten.nonEmpty, "RelationMentions need arguments")

  def this(
      label: String,
      arguments: Map[String, Seq[Mention]],
      paths: Map[String, Map[Mention, SynPath]],
      sentence: Int,
      document: Document,
      keep: Boolean,
      foundBy: String
  ) = this(Seq(label), mkTokenInterval(arguments), arguments, paths, sentence, document, keep, foundBy)

  def this(
      label: String,
      arguments: Map[String, Seq[Mention]],
      sentence: Int,
      document: Document,
      keep: Boolean,
      foundBy: String
  ) = this(Seq(label), mkTokenInterval(arguments), arguments, Map.empty[String, Map[Mention, SynPath]], sentence, document, keep, foundBy)

  def this(
      labels: Seq[String],
      arguments: Map[String, Seq[Mention]],
      sentence: Int,
      document: Document,
      keep: Boolean,
      foundBy: String
  ) = this(labels, mkTokenInterval(arguments), arguments, Map.empty[String, Map[Mention, SynPath]], sentence, document, keep, foundBy)

  def jsonAST: JValue = {
    val args = arguments.map {
      case (name, mentions) => (name -> JArray(mentions.map(_.jsonAST).toList))
    }
    ("type" -> "Relation") ~
    ("labels" -> labels) ~
    ("sentence" -> sentence) ~
    ("foundBy" -> foundBy) ~
    ("arguments" -> JObject(args.toList))
  }

  // Copy constructor for RelationMention
  def copy(
      labels: Seq[String] = this.labels,
      tokenInterval: Interval = this.tokenInterval,
      arguments: Map[String, Seq[Mention]] = this.arguments,
      paths: Map[String, Map[Mention, SynPath]] = this.paths,
      sentence: Int = this.sentence,
      document: Document = this.document,
      keep: Boolean = this.keep,
      foundBy: String = this.foundBy
  ): RelationMention = new RelationMention(labels, tokenInterval, arguments, paths, sentence, document, keep, foundBy)

  // Convert a RelationMention to an EventMention by specifying a trigger
  def toEventMention(trigger: TextBoundMention): EventMention = {

    require(trigger.document == this.document, "Trigger's document does not match RelationMention's document")
    require(trigger.sentence == this.sentence, "Trigger's sentence does not match RelationMention's sentence")

    new EventMention(
      this.labels,
      mkTokenInterval(trigger, this.arguments), // make new tokenInterval
      trigger,
      this.arguments,
      this.paths,
      this.sentence,
      this.document,
      this.keep,
      s"${this.foundBy} + toEventMention"
    )
  }

  // scatters the args named `argName` into N mentions each with `size` args named `argName`
  // all combinations of args are produced
  def scatter(argName: String, size: Int): Seq[RelationMention] =
    arguments
      .getOrElse(argName, Nil)
      .combinations(size)
      .map(args => this + (argName -> args))
      .toList

  // Create a new RelationMention by removing a single argument
  def -(argName: String): RelationMention =
    copy(arguments = this.arguments - argName)

  // Create a new RelationMention by removing a sequence of arguments
  def --(argNames: Seq[String]): RelationMention =
    copy(arguments = this.arguments -- argNames)

  // Create a new RelationMention by adding a key, value pair to the arguments map
  def +(arg: (String, Seq[Mention])): RelationMention =
    copy(arguments = this.arguments + arg)

}
