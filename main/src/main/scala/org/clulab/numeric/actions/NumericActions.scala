package org.clulab.numeric.actions

import org.clulab.odin.{Actions, Mention, State}
import org.clulab.numeric.mentions._

import scala.collection.mutable.ArrayBuffer

class NumericActions extends Actions {
  //
  // local actions
  //

  /** Constructs a DateMention from a token pattern */
  def mkDateMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    mentions.map(_.toDateMention)
  }

  /** Constructs a DateMention from the yyyy-mm-dd single token */
  def mkDateMentionYyyyMmDd(mentions: Seq[Mention], state: State): Seq[Mention] = {
    mentions.map(_.toDateMentionYyyyMmDd)
  }

  /** Constructs a DateMention from the dd-mm-yyyy single token */
  def mkDateMentionDdMmYyyy(mentions: Seq[Mention], state: State): Seq[Mention] = {
    mentions.map(_.toDateMentionDdMmYyyy)
  }

  //
  // global actions below this points
  //

  /** Global action for the numeric grammar */
  def cleanupAction(mentions: Seq[Mention], state: State): Seq[Mention] = {
    val r1 = keepLongestDates(mentions)

    r1
  }

  /** Keeps a date mention only if it is not contained in another */
  def keepLongestDates(mentions: Seq[Mention]): Seq[Mention] = {
    val dates = mentions.filter(m => m.isInstanceOf[DateMention])

    val filteredDates = new ArrayBuffer[Mention]()
    for(date <- dates) {
      var foundContainer = false
      for(m <- dates if m != date && ! foundContainer) {
        if(m.sentence == date.sentence && m.tokenInterval.contains(date.tokenInterval)) {
          foundContainer = true
        }
      }
      if(! foundContainer) {
        filteredDates += date
      }
    }

    val filteredMentions = new ArrayBuffer[Mention]()
    filteredMentions ++= filteredDates
    filteredMentions ++= mentions.filterNot(_.isInstanceOf[DateMention])

    filteredMentions
  }
}
