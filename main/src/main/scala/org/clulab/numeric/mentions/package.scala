package org.clulab.numeric

import org.clulab.odin.{Mention, RelationMention, TextBoundMention}
import java.util.regex.Pattern

package object mentions {
  val RANGE_SEP = " -- "

  def toNumberRangeMention(mention: Mention): NumberRangeMention =  mention match {
    case m: NumberRangeMention => m

    case m: RelationMention =>
      val number1Norm = getArgNorm("number1", m)
      if(number1Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument number1 in mention [${m.raw.mkString(" ")}]!")
      val number2Norm = getArgNorm("number2", m)
      if(number2Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument number2 in mention [${m.raw.mkString(" ")}] where number1Norm is [${number1Norm.get}]!")

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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to NumberRangeMention!")
  }

  def toMeasurementMention(mention: Mention): MeasurementMention =  mention match {
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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to MeasurementMention!")
  }

  def toPercentageMention(mention: Mention): PercentageMention =  mention match {
    case m:  PercentageMention => m

    case m: RelationMention =>
      new PercentageMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        getArgWords("number", m)
      )

    case m =>
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to PercentageMention!")
  }

  def toMeasurementWithRangeMention(mention: Mention): MeasurementMention =  mention match {
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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to MeasurementMention!")
  }

  def toDateRangeMention(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val date1Norm = getArgNorm("date1", m)
      if(date1Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date1 in mention [${m.raw.mkString(" ")}]!")
      val date2Norm = getArgNorm("date2", m)
      if(date2Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date2 in mention [${m.raw.mkString(" ")}]!")

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

  def toDateRangeMentionWithNumber(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val numberNorm = getArgWords("number", m)
      if(numberNorm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument number in mention [${m.raw.mkString(" ")}]!")
      val numberVal = NumberParser.parse(numberNorm.get)
      if(numberVal.isEmpty)
        throw new RuntimeException(s"ERROR: could not parse number [${numberNorm.get.mkString(" ")}] in mention [${m.raw.mkString(" ")}]!")
      val date2Norm = getArgNorm("date2", m)
      if(date2Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date2 in mention [${m.raw.mkString(" ")}]!")

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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to DateRangeMention!")
  }

  def toDateRangeMentionWithMonth(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val yearNorm = getArgWords("year", m)
      if(yearNorm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument number in mention [${m.raw.mkString(" ")}]!")

      val month1Norm = getArgWords("month1", m)
      if(month1Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument number in mention [${m.raw.mkString(" ")}]!")

      val month2Norm = getArgWords("month2", m)
      if(month2Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument number in mention [${m.raw.mkString(" ")}]!")

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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to DateRangeMention!")
  }

  def toDateRangeMentionWithSinceRef(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val date1Norm = getArgNorm("date1", m)
      if(date1Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date1 in mention [${m.raw.mkString(" ")}]!")

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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to DateRangeMention!")
  }

  def toDateRangeMentionWithUntilRef(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val date1Norm = getArgNorm("date1", m)
      if(date1Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date1 in mention [${m.raw.mkString(" ")}]!")

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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to DateRangeMention!")
  }

  def toDateUnboundRangeMentionBefore(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val date1Norm = getArgNorm("date1", m)
      if(date1Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date1 in mention [${m.raw.mkString(" ")}]!")

      new DateRangeMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        "XXXX-XX-XX",
        date1Norm.get
      )

    case m =>
      throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
  }

  def toDateUnboundRangeMentionAfter(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val date1Norm = getArgNorm("date1", m)
      if(date1Norm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date1 in mention [${m.raw.mkString(" ")}]!")

      new DateRangeMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        date1Norm.get,
        "XXXX-XX-XX"
      )

    case m =>
      throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
  }

  def toDateRangeMentionWithSeason(seasonNormalizer: SeasonNormalizer)(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val seasonNorm = getSeasonMonthRange(seasonNormalizer)("season", m)
      if(seasonNorm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument season in mention ${m.raw.mkString(" ")}!")

      val yearNorm = getArgWords("year", m)

      val (yearStart, yearEnd) = yearNorm match {
        case Some(year) =>
          val adjustedRange = seasonNormalizer.adjustYearRange(seasonNorm.get, year)
          (Some(adjustedRange._1), Some(adjustedRange._2))
        case _ => (None, None)
      }

      new DateRangeMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        TempEvalFormatter.mkDate(seasonNorm.get.startDay, seasonNorm.get.startMonth,yearStart),
        TempEvalFormatter.mkDate(seasonNorm.get.endDay, seasonNorm.get.endMonth, yearEnd)
      )

    case m =>
      throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
  }

  def toDateRangeMentionWithSeasons(seasonNormalizer: SeasonNormalizer)(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val seasonNorm1 = getSeasonMonthRange(seasonNormalizer)("season1", m)
      if(seasonNorm1.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument season1 in mention ${m.raw.mkString(" ")}!")

      val seasonNorm2 = getSeasonMonthRange(seasonNormalizer)("season2", m)
      if(seasonNorm2.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument season2 in mention ${m.raw.mkString(" ")}!")

      val yearNorm2 = getArgWords("year2", m)

      val yearNorm1 = getArgWords("year1", m) match {
        case Some(year) => Some(year)
        case _ if yearNorm2.isDefined => yearNorm2
        case _ => None
      }

      val yearEnd1 = yearNorm1 match {
        case Some(year) =>
          val adjustedRange = seasonNormalizer.adjustYearRange(seasonNorm1.get, year)
          Some(adjustedRange._2)
        case _ => None
      }

      val yearStart2 = yearNorm2 match {
        case Some(year) =>
          val adjustedRange = seasonNormalizer.adjustYearRange(seasonNorm2.get, year)
          Some(adjustedRange._1)
        case _ => None
      }

      new DateRangeMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        TempEvalFormatter.mkDate(seasonNorm1.get.endDay, seasonNorm1.get.endMonth, yearEnd1),
        TempEvalFormatter.mkDate(seasonNorm2.get.startDay, seasonNorm2.get.startMonth, yearStart2)
      )

    case m =>
      throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
  }

  def toDateRangeMentionWithSeasonSinceRef(seasonNormalizer: SeasonNormalizer)(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val seasonNorm = getSeasonMonthRange(seasonNormalizer)("season", m)
      if(seasonNorm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument season in mention ${m.raw.mkString(" ")}!")

      val yearNorm = getArgWords("year", m) match {
        case Some(year) =>
          val (_, yearEnd) = seasonNormalizer.adjustYearRange(seasonNorm.get, year)
          Some(yearEnd)
        case _ => None
      }

      new DateRangeMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        TempEvalFormatter.mkDate(seasonNorm.get.endDay, seasonNorm.get.endMonth, yearNorm),
        "ref-date"
      )

    case m =>
      throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
  }

  def toDateRangeMentionWithSeasonUntilRef(seasonNormalizer: SeasonNormalizer)(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val seasonNorm = getSeasonMonthRange(seasonNormalizer)("season", m)
      if(seasonNorm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument season in mention ${m.raw.mkString(" ")}!")

      val yearNorm = getArgWords("year", m) match {
        case Some(year) =>
          val (yearStart, _) = seasonNormalizer.adjustYearRange(seasonNorm.get, year)
          Some(yearStart)
        case _ => None
      }

      new DateRangeMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        "ref-date",
        TempEvalFormatter.mkDate(seasonNorm.get.startDay, seasonNorm.get.startMonth, yearNorm)
      )

    case m =>
      throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
  }

  def toDateUnboundRangeMentionWithSeasonBefore(seasonNormalizer: SeasonNormalizer)(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val seasonNorm = getSeasonMonthRange(seasonNormalizer)("season", m)
      if(seasonNorm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument season in mention ${m.raw.mkString(" ")}!")

      val yearNorm = getArgWords("year", m) match {
        case Some(year) =>
          val (yearStart, _) = seasonNormalizer.adjustYearRange(seasonNorm.get, year)
          Some(yearStart)
        case _ => None
      }

      new DateRangeMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        "XXXX-XX-XX",
        TempEvalFormatter.mkDate(seasonNorm.get.startDay, seasonNorm.get.startMonth, yearNorm)
      )

    case m =>
      throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
  }

  def toDateUnboundRangeMentionWithSeasonAfter(seasonNormalizer: SeasonNormalizer)(mention: Mention): DateRangeMention =  mention match {
    case m: DateRangeMention => m

    case m: RelationMention =>
      val seasonNorm = getSeasonMonthRange(seasonNormalizer)("season", m)
      if(seasonNorm.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument season in mention ${m.raw.mkString(" ")}!")

      val yearNorm = getArgWords("year", m) match {
        case Some(year) =>
          val (_, yearEnd) = seasonNormalizer.adjustYearRange(seasonNorm.get, year)
          Some(yearEnd)
        case _ => None
      }

      new DateRangeMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        TempEvalFormatter.mkDate(seasonNorm.get.endDay, seasonNorm.get.endMonth, yearNorm),
        "XXXX-XX-XX"
      )

    case m =>
      throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateRangeMention!")
  }

  def toDateMention(mention: Mention): DateMention =  mention match {
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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionYyyyMmDd(mention: Mention): DateMention =  mention match {
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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionDdMmYyyy(mention: Mention): DateMention =  mention match {
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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionYyMmDd(mention: Mention): DateMention =  mention match {
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
      throw new RuntimeException(s"ERROR: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionMmYyyy(mention: Mention): DateMention = mention match {
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
      throw new RuntimeException(s"Error: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionYyyyMm(mention: Mention): DateMention = mention match {
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
      throw new RuntimeException(s"Error: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionYyMm(mention: Mention): DateMention = mention match {
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
      throw new RuntimeException(s"Error: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionHoliday(mention: Mention): DateMention = mention match {
    case m: DateMention => m

    case m: RelationMention =>
      val holiday = getArgWords("holiday", m)
      if(holiday.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument holiday in mention ${m.raw.mkString(" ")}!")

      val year = getArgWords("year", m)

      val (day, month) = getHoliday(holiday.get, year)

      new DateMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        day, month, year
      )

    case m =>
      throw new RuntimeException(s"Error: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionWithModifierApprox(mention: Mention): DateMention = mention match {
    case m: DateMention => m

    case m: RelationMention =>
      val date = getArgNorm("date", m)
      if(date.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date in mention [${m.raw.mkString(" ")}]!")

      val approxSymbol = Some(ModifierNormalizer.APPROX_SYMBOL)
      val modifiedDate = ModifierNormalizer.splitDate(date.get)

      new DateMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        modifiedDate.day, modifiedDate.month, modifiedDate.year, approxSymbol
      )

    case m =>
      throw new RuntimeException(s"Error: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionWithModifierStart(mention: Mention): DateMention = mention match {
    case m: DateMention => m

    case m: RelationMention =>
      val date = getArgNorm("date", m)
      if(date.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date in mention [${m.raw.mkString(" ")}]!")

      val approxSymbol = if (ModifierNormalizer.isApprox(m.words.head))
        Some(ModifierNormalizer.APPROX_SYMBOL)
      else
        None
      val modifiedDate = ModifierNormalizer.startOf(date.get)

      new DateMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        modifiedDate.day, modifiedDate.month, modifiedDate.year, approxSymbol
      )

    case m =>
      throw new RuntimeException(s"Error: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionWithModifierMid(mention: Mention): DateMention = mention match {
    case m: DateMention => m

    case m: RelationMention =>
      val dateOpt = getArgNorm("date", m)
      if(dateOpt.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date in mention [${m.raw.mkString(" ")}]!")

      val approxSymbol = if (ModifierNormalizer.isApprox(m.words.head))
        Some(ModifierNormalizer.APPROX_SYMBOL)
      else
        None
      val modifiedDate = ModifierNormalizer.midOf(dateOpt.get)

      new DateMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        modifiedDate.day, modifiedDate.month, modifiedDate.year, approxSymbol
      )

    case m =>
      throw new RuntimeException(s"Error: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
  }

  def toDateMentionWithModifierEnd(mention: Mention): DateMention = mention match {
    case m: DateMention => m

    case m: RelationMention =>
      val date = getArgNorm("date", m)
      if(date.isEmpty)
        throw new RuntimeException(s"ERROR: could not find argument date in mention [${m.raw.mkString(" ")}]!")

      val approxSymbol = if (ModifierNormalizer.isApprox(m.words.head))
        Some(ModifierNormalizer.APPROX_SYMBOL)
      else
        None
      val modifiedDate = ModifierNormalizer.endOf(date.get)

      new DateMention(
        m.labels,
        m.tokenInterval,
        m.sentence,
        m.document,
        m.keep,
        m.foundBy,
        m.attachments,
        modifiedDate.day, modifiedDate.month, modifiedDate.year, approxSymbol
      )

    case m =>
      throw new RuntimeException(s"Error: cannot convert mention of type [${m.getClass.toString}] to DateMention!")
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

  private def getSeasonMonthRange(seasonNormalizer: SeasonNormalizer)(argName: String, m: Mention): Option[SeasonRange] = {
    val wordsOpt = getArgWords(argName, m)

    if (wordsOpt.isEmpty) None
    else seasonNormalizer.norm(wordsOpt.get)
  }

  private def getHoliday(holiday: Seq[String], year: Option[Seq[String]]): (Option[Seq[String]], Option[Seq[String]]) = {
    val dayMonthOpt = HolidayNormalizer.norm(holiday, year)
    dayMonthOpt match {
      case Some((day, month)) =>
        (Some(Seq(day)), Some(Seq(month)))
      case _ => throw new RuntimeException(s"ERROR: cannot get date for ${holiday.mkString(" ")}!")
    }
  }

  private def getArgWords(argName: String, m:Mention): Option[Seq[String]] =
      m.arguments.get(argName).map(_.head.words)

  private def getArgNorm(argName: String, m:Mention): Option[String] = {
    val argOpt = m.arguments.get(argName).map(_.head)

    argOpt match {
      case None => None
      case Some(norm: Norm) => Some(norm.neNorm)
      case Some(tbm: TextBoundMention) if tbm.labels.contains("Number") =>
        // Numbers are normalized on demand, for efficiency (so we do not create too many custom Mentions)
        NumberParser.parse(tbm.words).map(_.toString)
      case _ => None
    }
  }
}
