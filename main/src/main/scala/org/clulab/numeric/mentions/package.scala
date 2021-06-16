package org.clulab.numeric

import org.clulab.odin.{Mention, RelationMention, TextBoundMention}

import java.util.regex.Pattern

package object mentions {
  implicit class MentionOps(mention: Mention) {

    def toDateMention: DateMention =  mention match {
      case m: DateMention => m

      case m: RelationMention =>
        new DateMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          getArgWords("day", m),
          getArgWords("month", m),
          getArgWords("year", m)
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateMention!")
    }

    def toDateMentionYyyyMmDd: DateMention =  mention match {
      case m: DateMention => m

      case m: TextBoundMention =>
        val (year, month, day) = parseYyyyMmDd(m.words.head)
        new DateMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          Some(Seq(day)), Some(Seq(month)), Some(Seq(year))
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateMention!")
    }
  }

  // this can be more relaxed since the correct date format was previously checked by the Odin grammar
  private val DATE_YYYY_MM_DD: Pattern = Pattern.compile("(\\d+)\\D(\\d+)\\D(\\d+)")

  private def parseYyyyMmDd(v: String): (String, String, String) = {
    val m = DATE_YYYY_MM_DD.matcher(v)
    if(m.matches()) {
      val year = m.group(1)
      val month = m.group(2)
      val day = m.group(3)

      Tuple3(year, month, day)
    } else {
      throw new RuntimeException(s"ERROR: cannot extract year/month/day from date $v!")
    }
  }

  private def getArgWords(argName: String, m:Mention): Option[Seq[String]] = {
    if(! m.arguments.contains(argName)){
      None
    } else {
      val arg = m.arguments(argName).head
      Some(arg.words)
    }
  }
}
