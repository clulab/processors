package edu.arizona.sista.matcher

case class Interval(start: Int, end: Int) {
  require(start < end, "invalid range")

  override def toString = s"[$start,$end)"

  def toRange = start until end

  def contains(i: Int) = i >= start && i < end

  def contains(other: Interval) = start < other.start && end > other.end
  def isContainedBy(other: Interval) = other contains this

  def before(other: Interval) = end <= other.start
  def after(other: Interval) = start >= other.end
}

object Interval {
  def apply(idx: Int): Interval = Interval(idx, idx + 1)
}
