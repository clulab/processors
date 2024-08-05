package org.clulab.numeric

import org.clulab.sequences.CommentedStandardKbSource
import org.clulab.utils.Sourcer

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

case class NormAndUnitClass(norm: String, unitClassOpt: Option[String])

abstract class BaseUnitNormalizer() {
  def mkUnit(text: Seq[String]): String = text.mkString(" ").toLowerCase()
  def norm(text: Seq[String]): String
  def unitClassOpt(text: Seq[String]): Option[String]
}

object NullUnitNormalizer extends BaseUnitNormalizer() {
  def norm(text: Seq[String]): String = mkUnit(text)

  def unitClassOpt(text: Seq[String]): Option[String] = None
}

class UnitNormalizer(unitNormalizerPath: String) extends BaseUnitNormalizer() {
  val normMapper: Map[String, NormAndUnitClass] = UnitNormalizer.readNormsFromResource(unitNormalizerPath)

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
      Using.resource(Sourcer.sourceFromResource(path))(readNormsFromSource)

  def readNormsFromSource(source: Source): Map[String, NormAndUnitClass] = {
    val norms = new mutable.HashMap[String, NormAndUnitClass]()

    CommentedStandardKbSource.read(source) { (unit, normOpt, unitClassOpt) =>
      assert(normOpt.isDefined) // We're insisting on this.
      norms += unit -> NormAndUnitClass(normOpt.get, unitClassOpt)
    }
    norms.toMap
  }
}
