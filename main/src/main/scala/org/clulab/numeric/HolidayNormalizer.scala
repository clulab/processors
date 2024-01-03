package org.clulab.numeric

import de.jollyday.{Holiday, HolidayCalendar, HolidayManager, ManagerParameters}

import java.time.LocalDate
import scala.jdk.CollectionConverters._

object HolidayNormalizer {
  private val normMapper: Map[String, NormAndUnitClass] = UnitNormalizer.readNormsFromResource("/org/clulab/numeric/HOLIDAY.tsv")
  private val holidayManager = HolidayManager.getInstance(ManagerParameters.create(HolidayCalendar.UNITED_STATES))
  // Get keys for states in case there is ever a state-specific holiday.
  protected val stateKeys: Array[String] = holidayManager.getCalendarHierarchy.getChildren.keySet.asScala.toArray

  /** Retrieves date (day and month) for a holiday */
  def norm(holidaySeq: Seq[String], yearOpt: Option [Seq[String]]): Option[(String, String)] = {
    val holidayName = holidaySeq.mkString(" ").toLowerCase()
    val normAndUnitClassOpt = normMapper.get(holidayName)

    normAndUnitClassOpt.flatMap { normAndUnitClass =>
      val holidayCanonical = normAndUnitClass.norm
      // If year is None use current year as default.  This can lead to problems if the year is never filled in!
      // Do not run this code around around December 31 or January 1 because your results might be inconsistent.
      val year = yearOpt.map(_.mkString.toInt).getOrElse(LocalDate.now.getYear)
      // Get holidays from jollyday for the U.S. and all the states.
      val generalHolidayOpt = holidayManager.getHolidays(year).asScala
          .filter(_.getDescription == holidayCanonical).headOption
      val holidayOpt = generalHolidayOpt.orElse {
        val stateHolidays = stateKeys.flatMap(holidayManager.getHolidays(year, _).asScala)
            .filter(_.getDescription == holidayCanonical).distinct

        if (stateHolidays.length == 1) stateHolidays.headOption
        else None
      }
      val normOpt = holidayOpt.map { holiday =>
        val date = holiday.getDate

        (date.getDayOfMonth.toString, date.getMonthOfYear.toString)
      }

      normOpt
    }
  }
}
