package org.clulab.numeric

import org.clulab.odin.{Mention, RelationMention, TextBoundMention}

import java.util.regex.Pattern

package object mentions {
  implicit class MentionOps(mention: Mention) {

    def toMeasurementMention: MeasurementMention =  mention match {
      case m:  MeasurementMention => m

      case m: RelationMention =>
        new MeasurementMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          getArgWords("number", m),
          getArgWords("unit", m)
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to MeasurementMention!")
    }

    def toDateRangeMention: DateRangeMention =  mention match {
      case m: DateRangeMention => m

      case m: RelationMention =>
        val date1Norm = getArgNorm("date1", m)
        if(date1Norm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument date1 in mention ${m.raw.mkString(" ")}!")
        val date2Norm = getArgNorm("date2", m)
        if(date2Norm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument date2 in mention ${m.raw.mkString(" ")}!")

        new DateRangeMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          date1Norm.get,
          date2Norm.get
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
    }

    def toDateRangeMentionWithNumber: DateRangeMention =  mention match {
      case m: DateRangeMention => m

      case m: RelationMention =>
        val numberNorm = getArgWords("number", m)
        if(numberNorm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument number in mention ${m.raw.mkString(" ")}!")
        val numberVal = NumberParser.parse(numberNorm.get)
        if(numberVal.isEmpty)
          throw new RuntimeException(s"ERROR: could not parse number ${numberNorm.get.mkString(" ")} in mention ${m.raw.mkString(" ")}!")
        val date2Norm = getArgNorm("date2", m)
        if(date2Norm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument date2 in mention ${m.raw.mkString(" ")}!")

        new DateRangeMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          TempEvalFormatter.mkDate1Norm(numberVal.get, date2Norm.get),
          date2Norm.get
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
    }

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

    def toDateMentionDdMmYyyy: DateMention =  mention match {
      case m: DateMention => m

      case m: TextBoundMention =>
        val (year, month, day) = parseDdMmYyyy(m.words.head)
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

    def toDateMentionYyMmDd: DateMention =  mention match {
      case m: DateMention => m

      case m: TextBoundMention =>
        val (year, month, day) = parseYyMmDd(m.words.head)
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

    def toDateMentionMmYyyy: DateMention = mention match {
      case m: DateMention => m

      case m: TextBoundMention =>
        val (year, month) = parseMmYyyy(m.words.head)
        new DateMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          None, Some(Seq(month)), Some(Seq(year))
        )

      case m =>
        throw new RuntimeException(s"Error: cannot convert mention of type ${m.getClass.toString} to DateMention!")
    }

    def toDateMentionYyyyMm: DateMention = mention match {
      case m: DateMention => m

      case m: TextBoundMention =>
        val (year, month) = parseYyyyMm(m.words.head)
        new DateMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          None, Some(Seq(month)), Some(Seq(year))
        )

      case m =>
        throw new RuntimeException(s"Error: cannot convert mention of type ${m.getClass.toString} to DateMention!")
    }

    def toDateMentionYyMm: DateMention = mention match {
      case m: DateMention => m

      case m: TextBoundMention =>
        val (year, month) = parseYyMm(m.words.head)
        new DateMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          None, Some(Seq(month)), Some(Seq(year))
        )

      case m =>
        throw new RuntimeException(s"Error: cannot convert mention of type ${m.getClass.toString} to DateMention!")
    }
  }

  // this can be more relaxed since the correct date format was previously checked by the Odin grammar
  private val DATE_DD_DD_DD: Pattern = Pattern.compile("(\\d+)\\D(\\d+)\\D(\\d+)")

  // this can be more relaxed since the correct date format was previously checked by the Odin grammar
  // this is necessary for date formats such as MM-YYYY or YYYY-MM or YY-MM
  private val DATE_DD_DD: Pattern = Pattern.compile("(\\d+)\\D(\\d+)")

  private def parseYyyyMmDd(v: String): (String, String, String) = {
    val m = DATE_DD_DD_DD.matcher(v)
    if(m.matches()) {
      val year = m.group(1)
      val month = m.group(2)
      val day = m.group(3)

      Tuple3(year, month, day)
    } else {
      throw new RuntimeException(s"ERROR: cannot extract year/month/day from date $v!")
    }
  }

  private def parseYyMmDd(v: String): (String, String, String) = {
    val m = DATE_DD_DD_DD.matcher(v)
    if(m.matches()) {
      val year = m.group(1)
      val month = m.group(2)
      val day = m.group(3)

      Tuple3(year, month, day)
    } else {
      throw new RuntimeException(s"ERROR: cannot extract year/month/day from date $v!")
    }
  }

  private def parseDdMmYyyy(v: String): (String, String, String) = {
    val m = DATE_DD_DD_DD.matcher(v)
    if(m.matches()) {
      val day = m.group(1)
      val month = m.group(2)
      val year = m.group(3)

      Tuple3(year, month, day)
    } else {
      throw new RuntimeException(s"ERROR: cannot extract year/month/day from date $v!")
    }
  }

  private def parseMmYyyy(v:String): (String, String) = {
    val m = DATE_DD_DD.matcher(v)
    if(m.matches()) {
      val month = m.group(1)
      val year = m.group(2)

      Tuple2(year, month)
    } else {
      throw new RuntimeException(s"ERROR: cannot extract year/month/day from date $v!")
    }
  }


  private def parseYyyyMm(v:String): (String, String) = {
    val m = DATE_DD_DD.matcher(v)
    if(m.matches()) {
      val year = m.group(1)
      val month = m.group(2)

      Tuple2(year, month)
    } else {
      throw new RuntimeException(s"ERROR: cannot extract year/month/day from date $v!")
    }
  }

  private def parseYyMm(v:String): (String, String) = {
    val m = DATE_DD_DD.matcher(v)
    if(m.matches()) {
      val year = m.group(1)
      val month = m.group(2)

      Tuple2(year, month)
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

  private def getArgNorm(argName: String, m:Mention): Option[String] = {
    if(! m.arguments.contains(argName)){
      None
    } else {
      val arg = m.arguments(argName).head
      if(arg.isInstanceOf[Norm]) {
        Some(arg.asInstanceOf[Norm].neNorm)
      } else {
        None
      }
    }
  }
}
