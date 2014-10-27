package edu.arizona.sista.matcher

sealed trait Priority {
  def matches(i: Int): Boolean
}

case class ExactPriority(val value: Int) extends Priority {
  def matches(i: Int): Boolean = i == value
}

case class IntervalPriority(val start: Int, val end: Int) extends Priority {
  def matches(i: Int): Boolean = i >= start && i <= end
}

case class FromPriority(val from: Int) extends Priority {
  def matches(i: Int): Boolean = i >= from
}

object Priority {
  private val exact = """^(\d+)$""".r
  private val interval = """^(\d+)\s*-\s*(\d+)$""".r
  private val from = """^(\d+)\s*\+$""".r

  def apply(s: String): Priority = s.trim match {
    case exact(i) => new ExactPriority(i.toInt)
    case interval(start, end) => new IntervalPriority(start.toInt, end.toInt)
    case from(i) => new FromPriority(i.toInt)
    case _ => scala.sys.error("invalid priority string")
  }
}
