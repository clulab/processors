package org.clulab.numeric

import java.io.File

import org.clulab.utils.FileUtils

import scala.collection.mutable

object SeasonNormalizer {
  private val COMMENT = "//"

  private val normMapper = readNorms()

  private def getDayMonth(date: String): (Option[Seq[String]], Option[Seq[String]]) = {
    date.split("-") match {
      case Array(_, month, day) => (Some(Seq(month)), Some(Seq(day)))
      case Array(_, month) => (Some(Seq(month)), None)
      case _ => throw new RuntimeException(s"ERROR: incorrect date value in season file: $date")
    }
  }

  private def readNorms(): Map[String, SeasonRange] = {
    val norms = new mutable.HashMap[String, SeasonRange]()
    val resourcePath = "/org/clulab/numeric/SEASON.tsv"
    val customResourcePath = {
      val cwd = new File(System.getProperty("user.dir"), "src/main/resources/")
      new File(cwd, resourcePath)
    }
    val commentedText = if (customResourcePath.exists)
      FileUtils.getCommentedTextSetFromFile(customResourcePath)
    else
      FileUtils.getCommentedTextSetFromResource(resourcePath)

    for(line <- commentedText) {
      // the text before the comment (//) is the season name; the text after is the month range
      val commentStart = line.indexOf(COMMENT)
      assert(commentStart > 0 && commentStart < line.length)

      val season = line.substring(0, commentStart).trim
      val norm = line.substring(commentStart + COMMENT.length).split("--").map(_.trim)
      val normTuple = norm match {
        case Array(start, end) => (start, end)
        case _ => throw new RuntimeException(s"ERROR: incorrect date range in season file: $line")
      }

      val (startMonth, startDay) = getDayMonth(normTuple._1)
      val (endMonth, endDay) = getDayMonth(normTuple._2)

      norms += season -> SeasonRange(startDay, startMonth, endDay, endMonth)
    }

    norms.toMap
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