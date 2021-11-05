package org.clulab.numeric

import org.clulab.sequences.CommentedStandardKbSource
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import scala.collection.mutable
import scala.io.Source

object UnitNormalizer {
  private val normMapper = readNormsFromResource("/org/clulab/numeric/MEASUREMENT-UNIT.tsv")

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

  /** Normalizes measurement units */
  def norm(text: Seq[String]): String = {
    val unit = text.mkString(" ").toLowerCase()
    normMapper.getOrElse(unit, unit)
  }
}
