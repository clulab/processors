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

  // Change date1Norm if the date1Norm does not have a year value but date2Norm has a year value
  // This will handle date ranges of form month date and month date of year.
  // Ex: "Sowing dates between May 3 and October 2, 2020" will have a normalized value of 2020-05-03 -- 2020-10-02
  var d1 = date1Norm
  if (date1Norm contains "XXXX") {
    if (!(date2Norm contains "XXXX")) {
      // Keep everything from date1Norm except for the year value, which will be copied from date2Norm.
      d1 = date2Norm.substring(0,4) + date1Norm.substring(4)
    }
  }

  override def neNorm: String = {
    d1 + " -- " + date2Norm
    date1Norm + RANGE_SEP + date2Norm
  }

  override def neLabel: String = {
    "DATE-RANGE"
  }

}
