package org.clulab.numeric

import java.time.{Month, YearMonth}

import org.clulab.sequences.CommentedStandardKbSource
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import scala.collection.mutable
import scala.io.Source

object ModifierNormalizer {

  val APPROX_SYMBOL = "[APPROX]"
  private val APPROX_TOKENS = Array("around", "about")

  private def partOfTotal(total: Int, modifierPart: Double): String =
      "%02d".format(1.max((total * modifierPart).toInt))

  private def daysInMonth(month: String, yearOpt: Option[String]): Int = {
    yearOpt match {
      case Some(year) =>
        val yearMonthObj = YearMonth.of(year.toInt, month.toInt)
        yearMonthObj.lengthOfMonth()
      case None =>
        val monthObj = Month.of(month.toInt)
        monthObj.length(false)
    }
  }

  private def modifyDate(date: ModifiedDate, modifierPart: Double): ModifiedDate =
    date match {
      // matches dates of the type YYYY-MM-XX
      case ModifiedDate(Some(year), Some(month), None) =>
        val totalOfMonth = daysInMonth(month.mkString, Some(year.mkString))
        val day = partOfTotal(totalOfMonth, modifierPart)
        ModifiedDate(Some(year), Some(month), Some(Seq(day)))
      // matches dates of the type XXXX-MM-XX
      case ModifiedDate(None, Some(month), None) =>
        val totalOfMonth = daysInMonth(month.mkString, None)
        val day = partOfTotal(totalOfMonth, modifierPart)
        ModifiedDate(None, Some(month), Some(Seq(day)))
      // matches dates of the type YYYY-XX-XX
      case ModifiedDate(Some(year), None, None) =>
        val totalOfYear = 12
        val month = partOfTotal(totalOfYear, modifierPart)
        ModifiedDate(Some(year), Some(Seq(month)), None)
      // matches any other date, e.g. YYYY-MM-DD
      case _ =>
        date
    }

  private def partOf(date: String, modifierPart: Double): ModifiedDate = {
    val initialDate = splitDate(date)
    modifyDate(initialDate, modifierPart)
  }

  /** Gets the start of a date */
  def startOf(date: String): ModifiedDate = partOf(date, 0)

  /** Gets the middle of a date */
  def midOf(date: String): ModifiedDate = partOf(date, 0.5)

  /** Gets the end of a date */
  def endOf(date: String): ModifiedDate = partOf(date, 1)

  /** Returns true if possibleApprox is an approx modifier */
  def isApprox(possibleApprox: String): Boolean = APPROX_TOKENS.contains(possibleApprox)

  def splitDate(date: String): ModifiedDate = {
    val splitDate = date.split("-").map { m =>
      if (m.contains("X"))
        None
      else
        Some(Seq(m))
    }
    ModifiedDate(splitDate(0), splitDate(1), splitDate(2))
  }
}

case class ModifiedDate(year: Option[Seq[String]],
                        month: Option[Seq[String]],
                        day: Option[Seq[String]])