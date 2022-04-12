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
                         var date1Norm: String,
                         var date2Norm: String )
  extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy, attachments) with Norm {

  // Change date1Norm if the date1Norm is not undefined (i.e., XXXX-XX-XX) and
  // does not have a year value but date2Norm has a year value
  // This will handle date ranges of form month date and month date of year.
  // Ex: "Sowing dates between May 3 and October 2, 2020" will have a normalized value of 2020-05-03 -- 2020-10-02
  if (date1Norm != "XXXX-XX-XX" && date1Norm.startsWith("XXXX")) {
    if (!(date2Norm == "ref-date" || date2Norm.startsWith("XXXX"))) {
      // Keep everything from date1Norm except for the year value, which will be copied from date2Norm.
      date1Norm = date2Norm.substring(0,4) + date1Norm.substring(4)
    }
  }

  override def neNorm: String = {
    date1Norm + RANGE_SEP + date2Norm
  }

  override def neLabel: String = {
    "DATE-RANGE"
  }

}
