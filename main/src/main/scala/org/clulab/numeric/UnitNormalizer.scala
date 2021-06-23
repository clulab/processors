package org.clulab.numeric

object UnitNormalizer {
  /** Normalizes measurement units */
  def norm(text: Seq[String]): Option[String] = {
    // TODO: proper normalization (Mihai)
    Some(text.mkString(" ").toLowerCase())
  }
}
