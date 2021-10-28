package org.clulab.numeric

import org.clulab.odin.{Mention, RelationMention, TextBoundMention}

import java.util.regex.Pattern

package object mentions {
  val RANGE_SEP = " -- "

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
          getArgWords("unit", m),
          false
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to MeasurementMention!")
    }

    def toMeasurementWithRangeMention: MeasurementMention =  mention match {
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
          Some(Seq(getArgNorm("number", m).get)), // this has already been normalized in NumberRangeMention
          getArgWords("unit", m),
          true
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to MeasurementMention!")
    }

    def toNumberRangeMention: NumberRangeMention =  mention match {
      case m: NumberRangeMention => m

      case m: RelationMention =>
        val number1Norm = getArgNorm("number1", m)
        if(number1Norm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument number1 in mention ${m.raw.mkString(" ")}!")
        val number2Norm = getArgNorm("number2", m)
        if(number2Norm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument number2 in mention ${m.raw.mkString(" ")}!")

        new NumberRangeMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          number1Norm.get,
          number2Norm.get
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to NumberRangeMention!")
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

    def toDateRangeMentionWithMonth: DateRangeMention =  mention match {
      case m: DateRangeMention => m

      case m: RelationMention =>
        val yearNorm = getArgWords("year", m)
        if(yearNorm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument number in mention ${m.raw.mkString(" ")}!")

        val month1Norm = getArgWords("month1", m)
        if(month1Norm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument number in mention ${m.raw.mkString(" ")}!")

        val month2Norm = getArgWords("month2", m)
        if(month2Norm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument number in mention ${m.raw.mkString(" ")}!")

        new DateRangeMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          TempEvalFormatter.mkDate(None, month1Norm, yearNorm),
          TempEvalFormatter.mkDate(None, month2Norm, yearNorm)
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
    }

    def toDateRangeMentionWithSinceRef: DateRangeMention =  mention match {
      case m: DateRangeMention => m

      case m: RelationMention =>
        val date1Norm = getArgNorm("date1", m)
        if(date1Norm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument date1 in mention ${m.raw.mkString(" ")}!")

        new DateRangeMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          date1Norm.get,
          "ref-date"
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
    }

    def toDateRangeMentionWithUntilRef: DateRangeMention =  mention match {
      case m: DateRangeMention => m

      case m: RelationMention =>
        val date1Norm = getArgNorm("date1", m)
        if(date1Norm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument date1 in mention ${m.raw.mkString(" ")}!")

        new DateRangeMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          "ref-date",
          date1Norm.get
        )

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
    }

    def toDateRangeMentionWithSeason: DateRangeMention =  mention match {
      case m: DateRangeMention => m

      case m: RelationMention =>
        val seasonNorm = getSeasonMonthRange(m)
        if(seasonNorm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument season in mention ${m.raw.mkString(" ")}!")

        val yearNorm = getArgWords("year", m)
        if(yearNorm.isEmpty)
          throw new RuntimeException(s"ERROR: could not find argument year in mention ${m.raw.mkString(" ")}!")

        val (yearStart, yearEnd) = SeasonNormalizer.adjustYearRange(seasonNorm.get, yearNorm.get)

        new DateRangeMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments,
          TempEvalFormatter.mkDate(seasonNorm.get.startDay, seasonNorm.get.startMonth, Some(yearStart)),
          TempEvalFormatter.mkDate(seasonNorm.get.endDay, seasonNorm.get.endMonth, Some(yearEnd))
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

  private def getSeasonMonthRange(m: Mention): Option[SeasonRange] = {
    if(! m.arguments.contains("season"))
      None
    else {
      val season = m.arguments("season").head.words
      SeasonNormalizer.norm(season)
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
      } else if(arg.isInstanceOf[TextBoundMention] && arg.labels.contains("Number")) {
        // Numbers are normalized on demand, for efficiency (so we do not create too many custom Mentions)
        NumberParser.parse(arg.words).map(_.toString)
      } else {
        None
      }
    }
  }
}
