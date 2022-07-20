package org.clulab.sequences

// This is definitely not the most efficient as far as number of objects
// created, but there should be a NamedEntity thing to hold and not just
// shadows of it projected onto the BIO notation in an array of strings.
case class NamedEntity(label: String, range: Range) {

  def fill(bioLabels: Array[String]): Unit = {
    bioLabels(range.start) = NamedEntity.BEGIN + label
    if (range.length > 1) {
      val inner = NamedEntity.INSIDE + label

      range.drop(1).foreach(bioLabels(_) = inner)
    }
  }
}

object NamedEntity {
  val BEGIN = "B-"
  val INSIDE = "I-"
  val OUTSIDE = "O"

  def collect(bioLabels: IndexedSeq[String]): IndexedSeq[NamedEntity] = {

    def mkNamedEntity(label: String, begin: Int): NamedEntity = {
      // Start looking for the end one after the begin.
      val end = Range(begin + 1, bioLabels.length).find { index =>
        // We're done with the entity if a different one begins or we've landed outside...
        bioLabels(index).startsWith(BEGIN) || bioLabels(index) == OUTSIDE
      }.getOrElse(bioLabels.length) // ...or if we've come to the end.

      NamedEntity(label, Range(begin, end))
    }

    val labelAndBeginPairs = bioLabels.zipWithIndex.filter { case (label, _) => label.startsWith(BEGIN) }
    val namedEntities = labelAndBeginPairs.map { case (label, begin) => mkNamedEntity(label.drop(BEGIN.length), begin) }

    namedEntities
  }

  def combine(bioLabels: Array[String], genericNamedEntities: Seq[NamedEntity], customNamedEntities: Seq[NamedEntity]): Array[String] = {
    val genericRanges = genericNamedEntities.map(_.range).toSet

    customNamedEntities.foreach { customNamedEntity =>
      // If there is a matching range in the genericRanges, override the generic one with the
      // custom NamedEntity there.  Otherwise, stick with the generic one already in the result.
      if (genericRanges(customNamedEntity.range))
        customNamedEntity.fill(bioLabels)
    }

    bioLabels
  }
}
