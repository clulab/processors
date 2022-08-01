package org.clulab.numeric

import java.io.File

import org.clulab.sequences.CommentedStandardKbSource
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import scala.collection.mutable
import scala.io.Source

class SeasonNormalizer(seasonsPath: String) {
  val normMapper = SeasonNormalizer.readNormsFromResource(seasonsPath)

  /** Adjust the year start and end according to season date range */
  def adjustYearRange(seasonRange: SeasonRange, year: Seq[String]): (Seq[String], Seq[String]) = {
    val startMonthValue = seasonRange.startMonth.head.mkString(" ").toInt
    val endMonthValue = seasonRange.endMonth.head.mkString(" ").toInt
    endMonthValue < startMonthValue match {
      case true if 12 - startMonthValue >= endMonthValue =>
        val yearEnd = year.mkString("").toInt + 1
        (year, Seq(yearEnd.toString))
      case true if 12 - startMonthValue < endMonthValue =>
        val yearStart = year.mkString("").toInt - 1
        (Seq(yearStart.toString), year)
      case _ => (year, year)
    }
  }

  /** Normalizes seasons */
  def norm(text: Seq[String]): Option[SeasonRange] = {
    val season = text.mkString(" ").toLowerCase()
    normMapper.get(season)
  }
}

object SeasonNormalizer {

  def readNormsFromResource(path: String): Map[String, SeasonRange] = {
    val customResourcePath = new File(NumericEntityRecognizer.resourceDir, path)

    if (customResourcePath.exists)
      Sourcer.sourceFromFile(customResourcePath).autoClose(readNormsFromSource)
    else
      Sourcer.sourceFromResource(path).autoClose(readNormsFromSource)
  }

  def readNormsFromSource(source: Source): Map[String, SeasonRange] = {
    val norms = new mutable.HashMap[String, SeasonRange]()

    CommentedStandardKbSource.read(source) { (season, normOpt) =>
      assert(normOpt.isDefined) // We're insisting on this.

      val norm = normOpt.get.split("--").map(_.trim)
      val (start, end) = norm match {
        case Array(start, end) => (start, end)
        case _ => throw new RuntimeException(s"ERROR: incorrect date range in season file")
      }
      val (startMonth, startDay) = getDayMonth(start)
      val (endMonth, endDay) = getDayMonth(end)
      norms += season -> SeasonRange(startDay, startMonth, endDay, endMonth)
    }
    norms.toMap
  }

  private def getDayMonth(date: String): (Option[Seq[String]], Option[Seq[String]]) = {
    date.split("-") match {
      case Array(_, month, "XX") => (Some(Seq(month)), None)
      case Array(_, month, day) => (Some(Seq(month)), Some(Seq(day)))
      case _ => throw new RuntimeException(s"ERROR: incorrect date value in season file: $date")
    }
  }
}

case class SeasonRange(startDay: Option[Seq[String]],
                       startMonth: Option[Seq[String]],
                       endDay: Option[Seq[String]],
                       endMonth: Option[Seq[String]])