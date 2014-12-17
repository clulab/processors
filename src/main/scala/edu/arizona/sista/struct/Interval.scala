package edu.arizona.sista.struct

case class Interval(start: Int, end: Int) {
  require(start < end, "invalid range")

  override def toString: String = s"[$start,$end)"

  def size: Int = end - start

  def toSeq: Seq[Int] = start until end

  def contains(i: Int): Boolean = i >= start && i < end

  def intersects(that: Interval): Boolean =
    !(this.precedes(that) || this.meets(that) || this.metBy(that) || this.precededBy(that))

  // allen relations
  def precedes(that: Interval): Boolean = this.end < that.start
  def meets(that: Interval): Boolean = this.end == that.start
  def overlaps(that: Interval): Boolean = this.start < that.start && this.end > that.start && this.end < that.end
  def finishes(that: Interval): Boolean = this.start > that.start && this.end == that.end
  def contains(that: Interval): Boolean = this.start < that.start && this.end > that.end
  def starts(that: Interval): Boolean = this.start == that.start && this.end < that.end
  def equals(that: Interval): Boolean = this.start == that.start && this.end == that.end
  def startedBy(that: Interval): Boolean = that starts this
  def containedBy(that: Interval): Boolean = that contains this
  def finishedBy(that: Interval): Boolean = that finishes this
  def overlappedBy(that: Interval): Boolean = that overlaps this
  def metBy(that: Interval): Boolean = that meets this
  def precededBy(that: Interval): Boolean = that precedes this
}

object Interval {
  def apply(idx: Int): Interval = Interval(idx, idx + 1)
}
