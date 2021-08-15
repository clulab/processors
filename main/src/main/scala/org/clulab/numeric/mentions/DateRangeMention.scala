package org.clulab.numeric.mentions

import org.clulab.odin.{Attachment, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class DateRangeMention ( labels: Seq[String],
                         tokenInterval: Interval,
                         sentence: Int,
                         document: Document,
                         keep: Boolean,
                         foundBy: String,
                         attachments: Set[Attachment],
                         val date1Norm: String,
                         val date2Norm: String )
  extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy, attachments) with Norm {

  override def neNorm: String = {
    date1Norm + RANGE_SEP + date2Norm
  }

  override def neLabel: String = {
    "DATE-RANGE"
  }

}
