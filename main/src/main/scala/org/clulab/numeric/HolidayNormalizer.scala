package org.clulab.numeric

import de.jollyday.{Holiday, HolidayCalendar, HolidayManager, ManagerParameters}

import collection.JavaConverters._

object HolidayNormalizer {
  private val normMapper = UnitNormalizer.readNormsFromResource("/org/clulab/numeric/HOLIDAY.tsv")

  private val holidayManager = HolidayManager.getInstance(
    ManagerParameters.create(HolidayCalendar.UNITED_STATES)
  )

  /** Retrieves date (day and month) for a holiday */
  def norm(holidaySeq: Seq[String], yearOpt: Option [Seq[String]]): Option[(String, String)] = {
    val holiday = holidaySeq.mkString(" ").toLowerCase()
    normMapper.get(holiday) match {
      case Some(holidayCanonical) =>
        // If year is None use current year as default
        val year = yearOpt match {
          case Some(yearSeq) => yearSeq.mkString.toInt
          case _ => java.time.LocalDate.now.getYear
        }
        // Get holidays from jolliday for the U.S. and all the states.
        val holidays: Array[Holiday] = holidayManager.getCalendarHierarchy.getChildren.keySet.asScala
          .flatMap(holidayManager.getHolidays(year, _).asScala).toArray
        holidays.filter(_.getDescription == holidayCanonical) match {
          case Array(h) =>
            val date = h.getDate
            Some((date.getDayOfMonth.toString, date.getMonthValue.toString))
          case _ => None
        }
      case _ => None
    }
  }
}
