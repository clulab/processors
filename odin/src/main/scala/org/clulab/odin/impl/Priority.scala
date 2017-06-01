package org.clulab.odin.impl

sealed trait Priority {
  /** returns true if `i` matches the priority */
  def matches(i: Int): Boolean
  /** minimum number of iterations required to satisfy the priority */
  def minIterations: Int
}

case class ExactPriority(value: Int) extends Priority {
  def matches(i: Int): Boolean = i == value
  def minIterations: Int = value
}

case class IntervalPriority(start: Int, end: Int) extends Priority {
  def matches(i: Int): Boolean = i >= start && i <= end
  def minIterations: Int = end
}

case class LowerBoundPriority(start: Int) extends Priority {
  def matches(i: Int): Boolean = i >= start
  def minIterations: Int = start
}

case class SparsePriority(values: Set[Int]) extends Priority {
  def matches(i: Int): Boolean = values contains i
  def minIterations: Int = values.max
}

object Priority {
  private val exact = """^(\d+)$""".r
  private val interval = """^(\d+)\s*-\s*(\d+)$""".r
  private val lower = """^(\d+)\s*\+$""".r
  private val sparse = """^\[\s*(\d+(?:\s*,\s*\d+)*)\s*\]$""".r

  def apply(s: String): Priority = s.trim match {
    case exact(n) => ExactPriority(n.toInt)
    case interval(n, m) => IntervalPriority(n.toInt, m.toInt)
    case lower(n) => LowerBoundPriority(n.toInt)
    case sparse(ns) => SparsePriority(ns.split(",").map(_.trim.toInt).toSet)
    case p => sys.error(s"invalid priority '$p'")
  }
}
