package edu.arizona.sista.odin.impl

sealed trait Priority {
  def matches(i: Int): Boolean
  def startsAt: Int
}

case class ExactPriority(value: Int) extends Priority {
  def matches(i: Int): Boolean = i == value
  def startsAt: Int = value
}

case class IntervalPriority(start: Int, end: Int) extends Priority {
  def matches(i: Int): Boolean = i >= start && i <= end
  def startsAt: Int = start
}

case class InfiniteIntervalPriority(start: Int) extends Priority {
  def matches(i: Int): Boolean = i >= start
  def startsAt: Int = start
}

case class SparsePriority(values: Set[Int]) extends Priority {
  def matches(i: Int): Boolean = values contains i
  def startsAt: Int = values.min
}

object Priority {
  private val exact = """^(\d+)$""".r
  private val interval = """^(\d+)\s*-\s*(\d+)$""".r
  private val from = """^(\d+)\s*\+$""".r
  private val sparse = """^\[\s*(\d+(?:\s*,\s*\d+)*)\s*\]$""".r

  def apply(s: String): Priority = s.trim match {
    case exact(n) => ExactPriority(n.toInt)
    case interval(n, m) => IntervalPriority(n.toInt, m.toInt)
    case from(n) => InfiniteIntervalPriority(n.toInt)
    case sparse(ns) => SparsePriority(ns.split(",").map(_.trim.toInt).toSet)
    case p => sys.error(s"invalid priority '$p'")
  }
}
