package org.clulab.processors.clu

// This is the linguistic head, not the arrowhead!
// Rather than calling this constructor directly, go through HeadLabelScore.apply().
class HeadLabelScore protected (val head: Int, val label: String, val score: Float) {
  def toHeadLabel: HeadLabel = (head, label)
}

object HeadLabelScore {
  val SELF: Int = 0 // The relative head refers to itself.
  val ROOT: Int = -1 // The absolute head refers to root.

  // rel = relative, abs = absolute
  def newOpt(index: Int, validRange: Range, relHeadString: String, label: String, score: Float): Option[HeadLabelScore] = {
    val relHeadInt = relHeadString.toInt
    val absHeadIntOpt =
        if (relHeadInt == SELF) Some(ROOT)
        else validRange.lift(index + relHeadInt)

    absHeadIntOpt.map(new HeadLabelScore(_, label, score))
  }
}
