package org.clulab.numeric

import org.clulab.sequences.CommentedStandardKbSource
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import scala.collection.mutable
import scala.io.Source

object UnitNormalizer {
  private val normMapper = readNormsFromResource("/org/clulab/numeric/MEASUREMENT-UNIT.tsv")

  println(">>>" + normMapper + "<<<-")
  def readNormsFromResource(path: String): Map[String, Map[String, String]] =
      Sourcer.sourceFromResource(path).autoClose(readNormsFromSource)

  def readNormsFromSource(source: Source): Map[String, Map[String, String]] = {
    val norms = new mutable.HashMap[String, Map[String, String]]()

    CommentedStandardKbSource.read(source) { (unit, normOpt, unitClassOpt) =>
      assert(normOpt.isDefined) // We're insisting on this.
      val unitClass = if (unitClassOpt.isDefined) unitClassOpt.get else "UNK"
      norms += unit -> Map("norm" -> normOpt.get, "class" -> unitClass)
    }
    norms.toMap
  }

  /** Normalizes measurement units */
  def norm(text: Seq[String]): String = {
    val unit = text.mkString(" ").toLowerCase()
    if (normMapper.contains(unit)) {
      normMapper(unit)("norm")
    } else unit
  }

  /** Normalizes measurement units */
  def unitClass(text: Seq[String]): String = {
    val unit = text.mkString(" ").toLowerCase()
    if (normMapper.contains(unit)) {
      normMapper(unit)("class")
    } else "UNK"
  }
}
