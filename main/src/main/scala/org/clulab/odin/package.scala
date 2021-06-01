package org.clulab

import org.clulab.struct.Interval

package object odin {

  type Action = (Seq[Mention], State) => Seq[Mention]
  type SentenceToken = (Int, Int)
  type MentionLUT = Map[SentenceToken, Seq[Mention]]

  type SynPath = Seq[(Int, Int, String)] // governor, dependent, label

  def identityAction(mentions: Seq[Mention], state: State): Seq[Mention] = mentions


  // token interval that contains trigger and all matched arguments
  def mkTokenInterval(trigger: TextBoundMention, arguments: Map[String, Seq[Mention]]): Interval = {
    val allStarts = trigger.start +: arguments.values.flatMap(_.map(_.start)).toSeq
    val allEnds = trigger.end +: arguments.values.flatMap(_.map(_.end)).toSeq
    Interval(allStarts.min, allEnds.max)
  }

  // token interval that contains all matched arguments
  def mkTokenInterval(arguments: Map[String, Seq[Mention]]): Interval = {
    val allStarts = arguments.values.flatMap(_.map(_.start))
    val allEnds = arguments.values.flatMap(_.map(_.end))
    Interval(allStarts.min, allEnds.max)
  }

}
