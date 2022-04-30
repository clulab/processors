package org.clulab.numeric.mentions

import org.clulab.numeric.{NumberParser, UnitNormalizer}
import org.clulab.odin.{Attachment, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class PercentageMention ( labels: Seq[String],
                          tokenInterval: Interval,
                          sentence: Int,
                          document: Document,
                          keep: Boolean,
                          foundBy: String,
                          attachments: Set[Attachment],
                          val value: Option[Seq[String]])
  extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy, attachments) with Norm {

  override def neNorm: String = {
    assert(value.nonEmpty)

    val numValueOpt = NumberParser.parse(value.get)
    if(numValueOpt.isEmpty)
      throw new RuntimeException(s"ERROR: could not parse the number [${value.mkString(" ")}] in the percentage ${raw.mkString(" ")}!")

    numValueOpt.get + " %"
  }

  override def neLabel: String = {
    "PERCENTAGE"
  }
}
