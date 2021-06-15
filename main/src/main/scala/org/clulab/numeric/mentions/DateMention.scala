package org.clulab.numeric.mentions

import org.clulab.numeric.{NumberParser, TempEvalFormatter}
import org.clulab.odin.{Attachment, Mention, RelationMention, SynPath, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class DateMention ( labels: Seq[String],
                    tokenInterval: Interval,
                    arguments: Map[String, Seq[Mention]],
                    paths: Map[String, Map[Mention, SynPath]],
                    sentence: Int,
                    document: Document,
                    keep: Boolean,
                    foundBy: String,
                    attachments: Set[Attachment] )
  extends RelationMention(labels, tokenInterval, arguments, paths, sentence, document, keep, foundBy, attachments) with Norm {

  override def neNorm: String = {
    val day = getArgWords("day")
    val month = getArgWords("month")
    val year = getArgWords("year")
    TempEvalFormatter.mkDate(day, month, year)
  }

  private def getArgWords(argName: String): Option[Seq[String]] = {
    if(! arguments.contains(argName)){
      None
    } else {
      val arg = arguments(argName).head
      Some(arg.words)
    }

  }

  override def neLabel: String = {
    "DATE"
  }
}
