package edu.arizona.sista.odin

import scala.util.hashing.MurmurHash3._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

trait Mention extends Equals {
  def label: String
  def tokenInterval: Interval
  def sentence: Int
  def document: Document
  def keep: Boolean
  val arguments: Map[String, Seq[Mention]]

  // name of matching rule
  def foundBy: String

  def start: Int = tokenInterval.start
  def end: Int = tokenInterval.end

  // character offsets
  def startOffset: Int = document.sentences(sentence).startOffsets(start)
  def endOffset: Int = document.sentences(sentence).endOffsets(end - 1)

  // this method should be overriden by Mention subclases
  // to return the label and all the relevant labels in some taxonomy
  def allLabels: Set[String] = Set(label)

  // a mention matches a label if the label is returned by allLabels
  def matches(label: String): Boolean = allLabels contains label

  // returns all tokens in mention
  def words: Seq[String] = document.sentences(sentence).words.slice(start, end)
  def text: String = words.mkString(" ")

  override def canEqual(a: Any) = a.isInstanceOf[Mention]

  override def equals(that: Any): Boolean = that match {
    case that: Mention => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = {
    val h0 = symmetricSeed
    val h1 = mix(h0, label.hashCode)
    val h2 = mix(h1, tokenInterval.hashCode)
    val h3 = mix(h2, sentence.hashCode)
    val h4 = mix(h3, document.hashCode)
    val h5 = mixLast(h4, arguments.hashCode)
    finalizeHash(h5, 5)
  }
}

class TextBoundMention(val label: String,
                       val tokenInterval: Interval,
                       val sentence: Int,
                       val document: Document,
                       val keep: Boolean,
                       val foundBy: String) extends Mention {

  // TextBoundMentions don't have arguments
  val arguments: Map[String, Seq[Mention]] = Map.empty
}

class EventMention(val label: String,
                   val trigger: TextBoundMention,
                   val arguments: Map[String, Seq[Mention]],
                   val sentence: Int,
                   val document: Document,
                   val keep: Boolean,
                   val foundBy: String) extends Mention {
  // token interval that contains trigger and all matched arguments
  override def tokenInterval: Interval = {
    val allStarts = trigger.start +: arguments.values.flatMap(_.map(_.start)).toSeq
    val allEnds = trigger.end +: arguments.values.flatMap(_.map(_.end)).toSeq
    Interval(allStarts.min, allEnds.max)
  }
}

class RelationMention(val label: String,
                      val arguments: Map[String, Seq[Mention]],
                      val sentence: Int,
                      val document: Document,
                      val keep: Boolean,
                      val foundBy: String) extends Mention {
  require(arguments.values.flatten.nonEmpty, "RelationMentions need arguments")
  // token interval that contains all matched arguments
  override def tokenInterval: Interval = {
    val allStarts = arguments.values.flatMap(_.map(_.start)).toSeq
    val allEnds = arguments.values.flatMap(_.map(_.end)).toSeq
    Interval(allStarts.min, allEnds.max)
  }
}
