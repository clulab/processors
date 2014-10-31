package edu.arizona.sista.struct

case class Interval(start: Int, end: Int) {
  require(start < end, "invalid range")

  override def toString = s"[$start,$end)"

  def toRange = start until end

  def contains(i: Int) = i >= start && i < end

  def intersects(that: Interval) =
    !(this.precedes(that) || this.meets(that) || this.metBy(that) || this.precededBy(that))

  // allen relations
  def precedes(that: Interval) = this.end < that.start
  def meets(that: Interval) = this.end == that.start
  def overlaps(that: Interval) = this.start < that.start && this.end > that.start && this.end < that.end
  def finishes(that: Interval) = this.start > that.start && this.end == that.end
  def contains(that: Interval) = this.start < that.start && this.end > that.end
  def starts(that: Interval) = this.start == that.start && this.end < that.end
  def equals(that: Interval) = this.start == that.start && this.end == that.end
  def startedBy(that: Interval) = that starts this
  def containedBy(that: Interval) = that contains this
  def finishedBy(that: Interval) = that finishes this
  def overlappedBy(that: Interval) = that overlaps this
  def metBy(that: Interval) = that meets this
  def precededBy(that: Interval) = that precedes this
}

object Interval {
  def apply(idx: Int): Interval = Interval(idx, idx + 1)
}
