package org.clulab.numeric.actions

import org.clulab.odin.{Actions, Mention, State}
import org.clulab.numeric.mentions._
import org.clulab.scala.WrappedArrayBuffer._

import scala.collection.mutable.ArrayBuffer

class NumericActions extends Actions {
  //
  // local actions
  //

  /** Converts a sequence of mentions to new types given the converter function */
  private def convert(mentions: Seq[Mention], converter: Mention => Mention, converterName: String): Seq[Mention] = {
    val convertedMentions = new ArrayBuffer[Mention]()
    for(m <- mentions) {
      try {
        convertedMentions += converter(m)
      } catch {
        case e: Exception =>
          // sometimes these conversions fail, mainly on broken texts
          // let's be robust here: report the error and move on
          System.err.println(s"WARNING: $converterName conversion failed! Recovering and continuing...")
          e.printStackTrace()
      }
    }
    convertedMentions
  }

  /** Constructs a NumberRange mention from a token pattern */
  def mkNumberRangeMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toNumberRangeMention, "toNumberRangeMention")
  }

  /** Constructs a MeasurementMention from a token pattern */
  def mkMeasurementMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toMeasurementMention, "toMeasurementMention")
  }

  def mkMeasurementWithRangeMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toMeasurementWithRangeMention, "toMeasurementWithRangeMention")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMention, "toDateRangeMention")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithNumber(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithNumber, "toDateRangeMentionWithNumber")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithMonth(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithMonth, "toDateRangeMentionWithMonth")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithSinceRef(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithSinceRef, "toDateRangeMentionWithSinceRef")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithUntilRef(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithUntilRef, "toDateRangeMentionWithUntilRef")
  }

  /** Constructs a DateMention from a token pattern */
  def mkDateMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMention, "toDateMention")
  }

  /** Constructs a DateMention from the yyyy-mm-dd single token */
  def mkDateMentionYyyyMmDd(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionYyyyMmDd, "toDateMentionYyyyMmDd")
  }

  /** Constructs a DateMention from the dd-mm-yyyy single token */
  def mkDateMentionDdMmYyyy(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionDdMmYyyy, "toDateMentionDdMmYyyy")
  }

  /** Constructs a DateMention from the mm-yyyy single token */
  def mkDateMentionMmYyyy(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionMmYyyy, "toDateMentionMmYyyy")
  }

  /** Constructs a DateMention from the yyyy-mm single token */
  def mkDateMentionYyyyMm(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionYyyyMm, "toDateMentionYyyyMm")
  }

  /** Constructs a DateMention from the yy-mm single token */
  def mkDateMentionYyMm(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionYyMm, "toDateMentionYyMm")
  }

  /** Constructs a DateMention from the yy-mm single token */
  def mkDateMentionYyMmDd(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionYyMmDd, "toDateMentionYyMmDd")
  }

  //
  // global actions below this points
  //

  /** Global action for the numeric grammar */
  def cleanupAction(mentions: Seq[Mention], state: State): Seq[Mention] =
    cleanupAction(mentions)

  def cleanupAction(mentions: Seq[Mention]): Seq[Mention] = {
    val r1 = keepLongestMentions(mentions)
    r1
  }

  private def isNumeric(m: Mention): Boolean = {
    m.isInstanceOf[DateMention] ||
    m.isInstanceOf[DateRangeMention] ||
    m.isInstanceOf[MeasurementMention] ||
    m.isInstanceOf[NumberRangeMention]
  }

  /** Keeps a date (or date range) mention only if it is not contained in another */
  def keepLongestMentions(mentions: Seq[Mention]): Seq[Mention] = {
    val (numerics, nonNumerics) = mentions.partition(isNumeric)
    val filteredNumerics = numerics.filterNot { outerNumeric =>
      numerics.exists { innerNumeric =>
        innerNumeric != outerNumeric &&
        innerNumeric.sentence == outerNumeric.sentence &&
        innerNumeric.tokenInterval.contains(outerNumeric.tokenInterval)
      }
    }

    filteredNumerics ++ nonNumerics
  }
}
