package org.clulab.numeric

import de.jollyday.{Holiday, HolidayCalendar, HolidayManager, ManagerParameters}

import collection.JavaConverters._

object HolidayNormalizer {

  /** Retrieves date (day and month) for holiday */
  def norm(holidaySeq: Seq[String], yearOpt: Option [Seq[String]]): Option[(String, String)] = {
    val holidayManager = HolidayManager.getInstance(
      ManagerParameters.create(HolidayCalendar.UNITED_STATES)
    )
    val holiday = holidaySeq.mkString("_").toLowerCase()
    val year = yearOpt match {
      case Some(yearSeq) => yearSeq.mkString.toInt
      case _ => java.time.LocalDate.now.getYear
    }
    val holidays: Array[Holiday] = holidayManager.getHolidays(year).asScala.toArray
    holidays.filter(_.getPropertiesKey.toLowerCase == holiday) match {
      case Array(h) =>
        val date = h.getDate
        Some((date.getDayOfMonth.toString, date.getMonthValue.toString))
      case _ => None
    }
  }
}
