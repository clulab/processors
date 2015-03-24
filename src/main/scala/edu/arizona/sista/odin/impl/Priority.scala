package edu.arizona.sista.odin.impl

sealed trait Priority {
  def matches(i: Int): Boolean
}

case class ExactPriority(value: Int) extends Priority {
  def matches(i: Int): Boolean = i == value
}

case class IntervalPriority(start: Int, end: Int) extends Priority {
  def matches(i: Int): Boolean = i >= start && i <= end
}

case class InfiniteIntervalPriority(start: Int) extends Priority {
  def matches(i: Int): Boolean = i >= start
}

object Priority {
  private val exact = """^(\d+)$""".r
  private val interval = """^(\d+)\s*-\s*(\d+)$""".r
  private val from = """^(\d+)\s*\+$""".r

  def apply(s: String): Priority = s.trim match {
    case exact(n) => ExactPriority(n.toInt)
    case interval(n, m) => IntervalPriority(n.toInt, m.toInt)
    case from(n) => InfiniteIntervalPriority(n.toInt)
    case p => sys.error(s"invalid priority '$p'")
  }
}
