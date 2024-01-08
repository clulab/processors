package org.clulab.numeric

import de.jollyday.parameter.UrlManagerParameter
import de.jollyday.{HolidayCalendar, HolidayManager, ManagerParameters}

import java.io.File
import java.net.URL
import java.time.LocalDate
import java.util.Properties
import scala.jdk.CollectionConverters._

object HolidayNormalizer {
  protected val normMapper: Map[String, NormAndUnitClass] = UnitNormalizer.readNormsFromResource("/org/clulab/numeric/HOLIDAY.tsv")
  protected val holidayManager = {
    val xmlPath = "/home/kwa/Projects/clulab/processors-project/processors/main/src/main/resources/org/clulab/numeric/CluHolidays_sutime.xml"
    if (!new File(xmlPath).exists)
      println("It is wrong!")

    // Get the resource as url, check the protocol or something
    val holidayXmlUrl = new URL("file://" + xmlPath)
    val urlManagerParameter = new UrlManagerParameter(holidayXmlUrl, new Properties())
    val holidayManager = HolidayManager.getInstance(urlManagerParameter)

    holidayManager
  }

  /** Retrieves date (day and month) for a holiday */
  def norm(holidaySeq: Seq[String], yearOpt: Option [Seq[String]]): Option[(String, String)] = {
    val holidayName = holidaySeq.mkString(" ").toLowerCase()
    val normAndUnitClassOpt = normMapper.get(holidayName)

    normAndUnitClassOpt.flatMap { normAndUnitClass =>
      val holidayCanonical = normAndUnitClass.norm
      // If year is None use current year as default.  This can lead to problems if the year is never filled in!
      // Do not run this code around around December 31 or January 1 because your results might be inconsistent.
      val year = yearOpt.map(_.mkString.toInt).getOrElse(LocalDate.now.getYear)
      val allHolidays = holidayManager
          .getHolidays(year).asScala
      val holidayOpt = allHolidays
          .filter(_.getPropertiesKey == holidayCanonical)
          .headOption
      val normOpt = holidayOpt.map { holiday =>
        val date = holiday.getDate

        (date.getDayOfMonth.toString, date.getMonthOfYear.toString)
      }

      normOpt
    }
  }
}
