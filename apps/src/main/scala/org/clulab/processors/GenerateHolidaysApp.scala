package org.clulab.processors

import de.jollyday.{HolidayCalendar, HolidayManager, ManagerParameters}
import org.joda.time.LocalDate

import collection.JavaConverters._

object GenerateHolidaysApp extends App {
  val holidayManager = HolidayManager.getInstance(ManagerParameters.create(HolidayCalendar.UNITED_STATES))
  val stateKeys = holidayManager.getCalendarHierarchy.getChildren.keySet.asScala.toArray
  // We're only considering relatively modern holidays.
  val generalNameDescriptionDate = holidayManager.getHolidays(2021).asScala
      .map { holiday => (holiday.getPropertiesKey, holiday.getDescription, holiday.getDate) }
  val generalHolidays = generalNameDescriptionDate.map(_._1)
  val holidayNameDescriptionDateSeq = stateKeys
      .flatMap { stateKey =>
        val holidays = holidayManager.getHolidays(2021, stateKey).asScala.toArray

        holidays.map { holiday =>
          (holiday.getPropertiesKey, holiday.getDescription, holiday.getDate)
        }
      }
      .distinct
      .filterNot { case (state, _, _) =>
        generalHolidays.contains(state)
      }
      .sortBy(_._1)
  val duplicateHolidays = holidayNameDescriptionDateSeq
      .groupBy(_._1)
      .filter(_._2.length > 1)
      .map(_._1)
      .toSet
  val uniqueHolidayNameDescriptionDateSeq = holidayNameDescriptionDateSeq.filterNot { holidayName => duplicateHolidays(holidayName._1) }


  def printHoliday(nameDescriptionDate: (String, String, LocalDate)): Unit = {
    println(s"${nameDescriptionDate._1} // ${nameDescriptionDate._2}")
  }

  generalNameDescriptionDate.foreach(printHoliday)
  uniqueHolidayNameDescriptionDateSeq.foreach(printHoliday)
}
