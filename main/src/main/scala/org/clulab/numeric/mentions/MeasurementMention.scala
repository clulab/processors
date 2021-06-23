package org.clulab.numeric.mentions

import org.clulab.numeric.{NumberParser, UnitNormalizer}
import org.clulab.odin.{Attachment, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class MeasurementMention ( labels: Seq[String],
                           tokenInterval: Interval,
                           sentence: Int,
                           document: Document,
                           keep: Boolean,
                           foundBy: String,
                           attachments: Set[Attachment],
                           val value: Option[Seq[String]],
                           val unit: Option[Seq[String]] )
  extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy, attachments) with Norm {

  override def neNorm: String = {
    assert(value.nonEmpty)
    assert(unit.nonEmpty)

    val numValueOpt = NumberParser.parse(value.get)
    if(numValueOpt.isEmpty)
      throw new RuntimeException(s"ERROR: could not parse the number in the measurement ${raw.mkString(" ")}!")
    val unitNormOpt = UnitNormalizer.norm(unit.get)
    if(unitNormOpt.isEmpty)
      throw new RuntimeException(s"ERROR: could not normalize the unit in the measurement ${raw.mkString(" ")}!")

    numValueOpt.get + " " + unitNormOpt.get
  }

  override def neLabel: String = {
    "MEASUREMENT"
  }
}
