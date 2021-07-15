package org.clulab.numeric

import org.clulab.utils.FileUtils

import scala.collection.mutable

object UnitNormalizer {
  private val COMMENT = "//"

  private val normMapper = readNorms()

  private def readNorms(): Map[String, String] = {
    val norms = new mutable.HashMap[String, String]()

    for(line <- FileUtils.getCommentedTextSetFromResource("/org/clulab/numeric/MEASUREMENT-UNIT.tsv")) {
      // the text before the comment (//) is the unit name; the text after is the normalized unit name
      val commentStart = line.indexOf(COMMENT)
      assert(commentStart > 0 && commentStart < line.length)

      val unit = line.substring(0, commentStart).trim
      val norm = line.substring(commentStart + COMMENT.length).trim
      norms += unit -> norm
    }

    norms.toMap
  }

  /** Normalizes measurement units */
  def norm(text: Seq[String]): String = {
    val unit = text.mkString(" ").toLowerCase()
    normMapper.getOrElse(unit, unit)
  }
}
