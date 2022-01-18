package org.clulab.numeric

import java.time.{Month, YearMonth}

import org.clulab.sequences.CommentedStandardKbSource
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import scala.collection.mutable
import scala.io.Source

object ModifierNormalizer {

  val APPROX_SYMBOL = "[APPROX]"

  private def partOfYear(month: String, modifierType: String): String = {
    modifierType match {
      case "start" => "01"
      case "mid" => "06"
      case "end" => "12"
      case _ => throw new RuntimeException(s"ERROR: invalid modifier type $modifierType")
    }
  }

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

  private def partOfMonth(month: String, yearOpt: Option[String], modifierType: String): String = {
    modifierType match {
      case "start" => "01"
      case "mid" => (daysInMonth(month, yearOpt) / 2).toString
      case "end" => daysInMonth(month, yearOpt).toString
      case _ => throw new RuntimeException(s"ERROR: invalid modifier type $modifierType")
    }
  }

  private def modifyDate(date: ModifiedDate, modifierType: String): ModifiedDate =
    date match {
      case ModifiedDate(Some(year), Some(month), None) =>
        val day = partOfMonth(month.mkString, Some(year.mkString), modifierType)
        ModifiedDate(Some(year), Some(month), Some(Seq(day)))
      case ModifiedDate(None, Some(month), None) =>
        val day = partOfMonth(month.mkString, None, modifierType)
        ModifiedDate(None, Some(month), Some(Seq(day)))
      case ModifiedDate(Some(year), None, None) =>
        val month = partOfYear(year.mkString, modifierType)
        ModifiedDate(Some(year), Some(Seq(month)), None)
      case _ =>
        date
    }

  def readNormsFromResource(path: String): Map[String, String] =
      Sourcer.sourceFromResource(path).autoClose(readNormsFromSource)

  def readNormsFromSource(source: Source): Map[String, String] = {
    val norms = new mutable.HashMap[String, String]()

    CommentedStandardKbSource.read(source) { (unit, normOpt) =>
      assert(normOpt.isDefined) // We're insisting on this.
      norms += unit -> normOpt.get
    }
    norms.toMap
  }

  private def partOf(date: String, modifierType: String): ModifiedDate = {
    val initialDate = splitDate(date)
    modifyDate(initialDate, modifierType)
  }

  /** Gets the start of a date */
  def startOf(date: String): ModifiedDate = partOf(date, "start")

  /** Gets the middle of a date */
  def midOf(date: String): ModifiedDate = partOf(date, "mid")

  /** Gets the end of a date */
  def endOf(date: String): ModifiedDate = partOf(date, "end")

  /** Returns an APPROX tag if possibleApproxModifier is an approx modifier*/
  def isApprox(possibleModifier: String): Boolean = possibleModifier == "around"

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