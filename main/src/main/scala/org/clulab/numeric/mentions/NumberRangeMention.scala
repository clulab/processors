package org.clulab.numeric.mentions

import org.clulab.odin.{Attachment, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class NumberRangeMention ( labels: Seq[String],
                           tokenInterval: Interval,
                           sentence: Int,
                           document: Document,
                           keep: Boolean,
                           foundBy: String,
                           attachments: Set[Attachment],
                           val number1Norm: String,
                           val number2Norm: String )
  extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy, attachments) with Norm {

  override def neNorm: String = {
    number1Norm + RANGE_SEP + number2Norm
  }

  override def neLabel: String = {
    "NUMBER-RANGE"
  }

}
