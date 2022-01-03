package org.clulab.numeric

import java.time.{Month, YearMonth}

import org.clulab.sequences.CommentedStandardKbSource
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import scala.collection.mutable
import scala.io.Source

object ModifierNormalizer {
  private val normMapper = readNormsFromResource("/org/clulab/numeric/MODIFIER.tsv")

  private def partOfYear(month: String, modifierType: String): String = {
    modifierType match {
      case "start" => "01"
      case "mid" => "06"
      case "end" => "12"
      case _ => throw new RuntimeException(s"ERROR: invalidy modifier type $modifierType")
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
      case _ => throw new RuntimeException(s"ERROR: invalidy modifier type $modifierType")
    }
  }

  private def modifyDate(dateComponents: Array[Option[Seq[String]]], modifierType: String): ModifiedDate =
    dateComponents match {
      case Array(Some(year), Some(month), None) =>
        val day = partOfMonth(month.mkString, Some(year.mkString), modifierType)
        ModifiedDate(Some(year), Some(month), Some(Seq(day)), None)
      case Array(None, Some(month), None) =>
        val day = partOfMonth(month.mkString, None, modifierType)
        ModifiedDate(None, Some(month), Some(Seq(day)), None)
      case Array(Some(year), None, None) =>
        val month = partOfYear(year.mkString, modifierType)
        ModifiedDate(Some(year), Some(Seq(month)), None, None)
      case _ =>
        ModifiedDate(dateComponents(0), dateComponents(1), dateComponents(2), None)
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

  /** Normalizes dates with modifiers */
  def norm(date: String, modifier: String): ModifiedDate = {
    val dateComponents = date.split("-").map{ m =>
      if (m.contains("X"))
        None
      else
        Some(Seq(m))
    }
    normMapper.get(modifier) match {
      case Some(modifierType) if modifierType == "approx" =>
        ModifiedDate(dateComponents(0), dateComponents(1), dateComponents(2), Some("[APPROX]"))
      case Some(modifierType) =>
        modifyDate(dateComponents, modifierType)
      case None => throw new RuntimeException(s"ERROR: $modifier not found in modifier file")
    }
  }
}

case class ModifiedDate(day: Option[Seq[String]],
                        month: Option[Seq[String]],
                        year: Option[Seq[String]],
                        modifer: Option[String])