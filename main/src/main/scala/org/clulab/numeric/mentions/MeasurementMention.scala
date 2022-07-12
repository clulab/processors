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
                           val unit: Option[Seq[String]],
                           val fromRange: Boolean)
  extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy, attachments) with Norm {

  println("v: " + value.get.head)
  println("u: " + unit.get.head)
  override def neNorm: String = {
    println("here")
    assert(value.nonEmpty)
    assert(unit.nonEmpty)

    val numValueOpt =
      if(fromRange) {
        Some(value.get.head)
      } else {
        NumberParser.parse(value.get)
      }
    if(numValueOpt.isEmpty)
      throw new RuntimeException(s"ERROR: could not parse the number [${value.mkString(" ")}] in the measurement ${raw.mkString(" ")}!")
    val unitNorm = UnitNormalizer.norm(unit.get)
    println("Unit norm: " + unitNorm)
    numValueOpt.get + " " + unitNorm
  }

  override def neLabel: String = {
    "MEASUREMENT"
  }
}
