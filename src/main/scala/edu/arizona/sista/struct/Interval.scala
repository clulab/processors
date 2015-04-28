package edu.arizona.sista.struct

case class Interval(start: Int, end: Int) {
  require(start < end, "invalid range")

  def size: Int = end - start

  def toSeq: Seq[Int] = start until end

  def contains(i: Int): Boolean = i >= start && i < end

  def contains(that: Interval): Boolean =
    this.allenContains(that) || this.allenStartedBy(that) || this.allenFinishedBy(that)

  def overlaps(that: Interval): Boolean =
    !(this.allenPrecedes(that) || this.allenMeets(that) || this.allenMetBy(that) || this.allenPrecededBy(that))

  @deprecated("Please use Interval.overlaps instead", "processors 5.3")
  def intersects(that: Interval): Boolean = overlaps(that)

  // allen relations
  def allenPrecedes(that: Interval): Boolean = this.end < that.start
  def allenMeets(that: Interval): Boolean = this.end == that.start
  def allenOverlaps(that: Interval): Boolean = this.start < that.start && this.end > that.start && this.end < that.end
  def allenFinishes(that: Interval): Boolean = this.start > that.start && this.end == that.end
  def allenContains(that: Interval): Boolean = this.start < that.start && this.end > that.end
  def allenStarts(that: Interval): Boolean = this.start == that.start && this.end < that.end
  def allenEquals(that: Interval): Boolean = this.start == that.start && this.end == that.end
  def allenStartedBy(that: Interval): Boolean = that allenStarts this
  def allenContainedBy(that: Interval): Boolean = that allenContains this
  def allenFinishedBy(that: Interval): Boolean = that allenFinishes this
  def allenOverlappedBy(that: Interval): Boolean = that allenOverlaps this
  def allenMetBy(that: Interval): Boolean = that allenMeets this
  def allenPrecededBy(that: Interval): Boolean = that allenPrecedes this
}

object Interval {
  def apply(idx: Int): Interval = Interval(idx, idx + 1)
}
