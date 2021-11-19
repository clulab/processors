package org.clulab.numeric

import java.io.File

import org.clulab.sequences.CommentedStandardKbSource
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import scala.collection.mutable
import scala.io.Source

object SeasonNormalizer {
  private val normMapper = readNormsFromResource("/org/clulab/numeric/SEASON.tsv")

  def readNormsFromResource(path: String): Map[String, SeasonRange] = {
    val customResourcePath = {
      val cwd = new File(System.getProperty("user.dir"), "src/main/resources/")
      new File(cwd, path)
    }
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
      val normTuple = norm match {
        case Array(start, end) => (start, end)
        case _ => throw new RuntimeException(s"ERROR: incorrect date range in season file")
      }
      val (startMonth, startDay) = getDayMonth(normTuple._1)
      val (endMonth, endDay) = getDayMonth(normTuple._2)
      norms += season -> SeasonRange(startDay, startMonth, endDay, endMonth)
    }
    norms.toMap
  }

  private def getDayMonth(date: String): (Option[Seq[String]], Option[Seq[String]]) = {
    date.split("-") match {
      case Array(_, month, day) => (Some(Seq(month)), Some(Seq(day)))
      case Array(_, month) => (Some(Seq(month)), None)
      case _ => throw new RuntimeException(s"ERROR: incorrect date value in season file: $date")
    }
  }

  /** Adjust the year start and end according to season date range */
  def adjustYearRange(seasonRange: SeasonRange, year: Seq[String]): (Seq[String], Seq[String]) = {
    val startMonthValue = seasonRange.startMonth.head.mkString(" ").toInt
    val endMonthValue = seasonRange.endMonth.head.mkString(" ").toInt
    endMonthValue < startMonthValue match {
      case true if startMonthValue > 6 =>
        val yearStart = year.mkString("").toInt - 1
        (Seq(yearStart.toString), year)
      case true if startMonthValue <= 6 =>
        val yearEnd = year.mkString("").toInt + 1
        (year, Seq(yearEnd.toString))
      case _ => (year, year)
    }
  }

  /** Normalizes seasons */
  def norm(text: Seq[String]): Option[SeasonRange] = {
    val season = text.mkString(" ").toLowerCase()
    normMapper.get(season)
  }
}

case class SeasonRange(startDay: Option[Seq[String]],
                       startMonth: Option[Seq[String]],
                       endDay: Option[Seq[String]],
                       endMonth: Option[Seq[String]])