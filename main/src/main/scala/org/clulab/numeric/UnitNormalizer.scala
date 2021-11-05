package org.clulab.numeric

import org.clulab.sequences.CommentedStandardKbSource
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import scala.collection.mutable

object UnitNormalizer {
  private val normMapper = readNorms()

  protected def readNorms(): Map[String, String] = {
    val path = "/org/clulab/numeric/MEASUREMENT-UNIT.tsv"
    val norms = Sourcer.sourceFromResource(path).autoClose { source =>
      val norms = new mutable.HashMap[String, String]()
      
      CommentedStandardKbSource.read(source) { (unit, normOpt) =>
        assert(normOpt.isDefined) // We're insisting on this.
        norms += unit -> normOpt.get
      }
      norms
    }

    norms.toMap
  }

  /** Normalizes measurement units */
  def norm(text: Seq[String]): String = {
    val unit = text.mkString(" ").toLowerCase()
    normMapper.getOrElse(unit, unit)
  }
}
