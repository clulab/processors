package org.clulab.processors.clu

// This is the linguistic head, not the arrowhead!
// Rather than calling this constructor directly, go through HeadLabelScore.apply().
// This is a case class for automatic generation of equals.
case class HeadModLabelScore protected(protected val realHead: Int, protected val realMod: Int, label: String, score: Float) {
  def toHeadLabel: HeadLabel = (realHead, label)

  // For purposes of the Eisner algorithm, these values are offset from the "real" ones.
  // A small amount of code needs to know better.
  def head: Int = realHead + HeadModLabelScore.extension

  def mod: Int = realMod + HeadModLabelScore.extension
}

object HeadModLabelScore {
  val SELF: Int = 0 // The relative head refers to itself.
  val ROOT: Int = -1 // The absolute head refers to root.
  val extension: Int = 1

  // rel = relative, abs = absolute
  def newOpt(index: Int, validRange: Range, relHeadString: String, label: String, score: Float): Option[HeadModLabelScore] = {
    val relHeadInt = relHeadString.toInt
    val absHeadIntOpt =
        if (relHeadInt == SELF) Some(ROOT)
        else validRange.lift(index + relHeadInt)

    absHeadIntOpt.map(new HeadModLabelScore(_, index, label, score))
  }

  def toHeadLabels(headModLabelScores: Seq[HeadModLabelScore]): Array[HeadLabel] = {
    val headLabels = new Array[HeadLabel](headModLabelScores.length)

    // If these were sorted by mod, then we would have it, but
    // they aren't and this is faster.
    headModLabelScores.foreach { headModLabelScores =>
      headLabels(headModLabelScores.realMod) = headModLabelScores.toHeadLabel
    }
    headLabels
  }
}
