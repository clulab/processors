package org.clulab.processors.clu

// This is the linguistic head, not the arrowhead!
// Rather than calling this constructor directly, go through HeadLabelScore.apply().
case class Dependency protected(protected val realHead: Int, protected val realMod: Int, label: String, score: Float) {
  def toHeadLabel: HeadLabel = (realHead, label)

  def toHeadLabel(headLabels: Array[HeadLabel]): Unit = headLabels(realMod) = toHeadLabel

  // For purposes of the Eisner algorithm, these values are offset from the "real" ones.
  // A small amount of code needs to know better.
  def head: Int = realHead + Dependency.offset

  def mod: Int = realMod + Dependency.offset
}

object Dependency {
  val SELF: Int = 0 // The relative head refers to itself.
  val ROOT: Int = -1 // The absolute head refers to root.
  val offset: Int = 1

  // rel = relative, abs = absolute
  def newOpt(index: Int, validRange: Range, relHeadString: String, label: String, score: Float): Option[Dependency] = {
    val relHeadInt = relHeadString.toInt
    val absHeadIntOpt =
        if (relHeadInt == SELF) Some(ROOT)
        else validRange.lift(index + relHeadInt)

    absHeadIntOpt.map(new Dependency(_, index, label, score))
  }

  def toHeadLabels(dependencies: Seq[Dependency]): Array[HeadLabel] = {
    val headLabels = new Array[HeadLabel](dependencies.length)

    // If these were sorted by mod, then we would have it,
    // but they aren't and this is faster.
    dependencies.foreach { headModLabelScores =>
      headModLabelScores.toHeadLabel(headLabels)
    }
    headLabels
  }
}
