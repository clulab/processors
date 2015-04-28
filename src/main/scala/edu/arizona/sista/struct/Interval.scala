package edu.arizona.sista.struct

case class Interval(start: Int, end: Int) {
  require(start < end, "invalid range")

  def size: Int = end - start

  def toSeq: Seq[Int] = start until end

  def contains(i: Int): Boolean = i >= start && i < end

  def contains(that: Interval): Boolean =
    this.start <= that.start && this.end >= that.end

  def overlaps(that: Interval): Boolean =
    if (this.start < that.start) {
      this.end > that.start
    } else if (this.start > that.start) {
      this.start < that.end
    } else true

  @deprecated("Please use Interval.overlaps instead", "processors 5.3")
  def intersects(that: Interval): Boolean = overlaps(that)

  // A precedes B
  //
  // A: #####
  // B:       #####
  def allenPrecedes(that: Interval): Boolean =
    this.end < that.start

  // A meets B
  //
  // A: #####
  // B:      #####
  def allenMeets(that: Interval): Boolean =
    this.end == that.start

  // A overlaps B
  //
  // A: #####
  // B:    #####
  def allenOverlaps(that: Interval): Boolean =
    this.start < that.start && this.end > that.start && this.end < that.end

  // A finishes B
  //
  // A:      #####
  // B: ##########
  def allenFinishes(that: Interval): Boolean =
    this.start > that.start && this.end == that.end

  // A contains B
  //
  // A: ##########
  // B:    #####
  def allenContains(that: Interval): Boolean =
    this.start < that.start && this.end > that.end

  // A starts B
  //
  // A: #####
  // B: ##########
  def allenStarts(that: Interval): Boolean =
    this.start == that.start && this.end < that.end

  // A equals B
  //
  // A: #####
  // B: #####
  def allenEquals(that: Interval): Boolean =
    this.start == that.start && this.end == that.end

  // A startedBy B
  //
  // A: ##########
  // B: #####
  def allenStartedBy(that: Interval): Boolean =
    that allenStarts this

  // A containedBy B
  //
  // A:    #####
  // B: ##########
  def allenContainedBy(that: Interval): Boolean =
    that allenContains this

  // A finishedBy B
  //
  // A: ##########
  // B:      #####
  def allenFinishedBy(that: Interval): Boolean =
    that allenFinishes this

  // A overlappedBy B
  //
  // A:    #####
  // B: #####
  def allenOverlappedBy(that: Interval): Boolean =
    that allenOverlaps this

  // A metBy B
  //
  // A:      #####
  // B: #####
  def allenMetBy(that: Interval): Boolean =
    that allenMeets this

  // A precededBy B
  //
  // A:        #####
  // B: #####
  def allenPrecededBy(that: Interval): Boolean =
    that allenPrecedes this
}

object Interval {
  def apply(idx: Int): Interval = Interval(idx, idx + 1)
}
