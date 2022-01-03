package org.clulab.numeric.mentions

import org.clulab.numeric.TempEvalFormatter
import org.clulab.odin.{Attachment, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class DateMention ( labels: Seq[String],
                    tokenInterval: Interval,
                    sentence: Int,
                    document: Document,
                    keep: Boolean,
                    foundBy: String,
                    attachments: Set[Attachment],
                    val day: Option[Seq[String]],
                    val month: Option[Seq[String]],
                    val year: Option[Seq[String]],
                    val modifier: Option[String] = None)
  extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy, attachments) with Norm {

  override def neNorm: String = {
      TempEvalFormatter.mkDate(day, month, year, modifier)
  }

  override def neLabel: String = {
    "DATE"
  }
}
