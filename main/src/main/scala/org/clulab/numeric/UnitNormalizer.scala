package org.clulab.numeric

import org.clulab.sequences.CommentedStandardKbSource
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import scala.collection.mutable
import scala.io.Source

case class NormAndUnitClass(norm: String, unitClassOpt: Option[String])

class UnitNormalizer(unitNormalizerPath: String) {
  val normMapper: Map[String, NormAndUnitClass] = UnitNormalizer.readNormsFromResource(unitNormalizerPath)

  def mkUnit(text: Seq[String]): String = text.mkString(" ").toLowerCase()

  /** Normalizes measurement units */
  def norm(text: Seq[String]): String = {
    val unit = mkUnit(text)

    normMapper.get(unit).map(_.norm).getOrElse(unit)
  }

  /** Normalizes measurement units */
  def unitClassOpt(text: Seq[String]): Option[String] = {
    val unit = mkUnit(text)

    normMapper.get(unit).flatMap(_.unitClassOpt)
  }
}

object UnitNormalizer {
  private val normMapper = readNormsFromResource("/org/clulab/numeric/MEASUREMENT-UNIT.tsv")

  def readNormsFromResource(path: String): Map[String, NormAndUnitClass] =
      Sourcer.sourceFromResource(path).autoClose(readNormsFromSource)

  def readNormsFromSource(source: Source): Map[String, NormAndUnitClass] = {
    val norms = new mutable.HashMap[String, NormAndUnitClass]()

    CommentedStandardKbSource.read(source) { (unit, normOpt, unitClassOpt) =>
      assert(normOpt.isDefined) // We're insisting on this.
      norms += unit -> NormAndUnitClass(normOpt.get, unitClassOpt)
    }
    norms.toMap
  }
}
