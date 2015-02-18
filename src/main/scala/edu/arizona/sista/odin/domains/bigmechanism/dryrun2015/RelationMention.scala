package edu.arizona.sista.odin.domains.bigmechanism.dryrun2015

import edu.arizona.sista.odin.Mention
import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.Interval

/**
 * Created by gus on 12/19/14.
 */

class RelationMention
  (val label: String,
  val arguments: Map[String, Seq[Mention]],
  val sentence: Int,
  val document: Document,
  val keep: Boolean,
  val foundBy: String) extends Mention {
    require(arguments.values.flatten.nonEmpty, "RelationMentions need arguments")
    // token interval that contains trigger and all matched arguments
    override def tokenInterval: Interval = {
      val allStarts = arguments.values.flatMap(_.map(_.start)).toSeq
      val allEnds = arguments.values.flatMap(_.map(_.end)).toSeq
      Interval(allStarts.min, allEnds.max)
    }
    override def text: String = {
      if (label == "Complex") arguments("Participant").map(_.text).mkString("-")
      else words.mkString(" ")
    }
}
