package org.clulab.numeric

import de.jollyday.config.Configuration
import de.jollyday.impl.DefaultHolidayManager
import de.jollyday.parameter.UrlManagerParameter
import de.jollyday.{HolidayCalendar, HolidayManager, ManagerParameters}

import java.io.File
import java.net.URL
import java.time.LocalDate
import java.util.Properties
import scala.jdk.CollectionConverters._

class CluXmlManager extends DefaultHolidayManager {
  println("I got here!")

  // This exposes the otherwise protected configuration.
  def getConfiguration(): Configuration = configuration
}

object HolidayNormalizer {
  protected val normMapper: Map[String, NormAndUnitClass] = UnitNormalizer.readNormsFromResource("/org/clulab/numeric/HOLIDAY.tsv")
  protected val holidayManager =
      if (true) {
        val managerProps = {
          val properties = new Properties()

          properties.setProperty("manager.impl", "org.clulab.numeric.CluXmlManager")
          properties
        }
        val xmlPath = "/home/kwa/Projects/clulab/processors-project/processors/main/src/main/resources/CluHolidays_sutime.xml"
        if (!new File(xmlPath).exists)
          println("It is wrong!")
        val holidayXmlUrl = new URL("file://" + xmlPath)
        val urlManagerParameter = new UrlManagerParameter(holidayXmlUrl, managerProps)
        val holidayManager = HolidayManager.getInstance(urlManagerParameter)

        holidayManager
      }
      else {
        val holidayManager = HolidayManager.getInstance(ManagerParameters.create(HolidayCalendar.UNITED_STATES))

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
          .filter(_.getPropertiesKey == holidayCanonical).headOption
      val normOpt = holidayOpt.map { holiday =>
        val date = holiday.getDate

        (date.getDayOfMonth.toString, date.getMonthOfYear.toString)
      }

      normOpt
    }
  }
}
