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

  def isOutside(bioLabels: Array[String]): Boolean =
      range.forall(bioLabels(_) == NamedEntity.OUTSIDE)
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
    // Neither named entity sequence can contain overlapping elements within the sequence.
    // At most, there is overlap between sequences.  Use is made of that fact.
    val genericStarts = genericNamedEntities.map(_.range.start).toSet
    val genericEnds = genericNamedEntities.map(_.range.end).toSet

    customNamedEntities.foreach { customNamedEntity =>
      // If there is a matching range in the genericRanges, override the generic one with the
      // customNamedEntity there.  If the entire range is OUTSIDE, also take the custonNamedEntity.
      // Otherwise, stick with the generic one already in the bioLabels.
      if (
        (genericStarts(customNamedEntity.range.start) || bioLabels(customNamedEntity.range.start) == OUTSIDE) &&
        (genericEnds(customNamedEntity.range.end) || bioLabels(customNamedEntity.range.end - 1) == OUTSIDE)
      )
      customNamedEntity.fill(bioLabels)
    }

    bioLabels
  }

  def isValid(bioLabels: Array[String], index: Int): Boolean = {
    val currBioLabel = bioLabels(index)
    !currBioLabel.startsWith(INSIDE) || {
      0 < index && {
        val prevBioLabel = bioLabels(index - 1)
        prevBioLabel == currBioLabel || {
          prevBioLabel == toBegin(currBioLabel)
        }
      }
    }
  }

  def isValid(bioLabels: Array[String]): Boolean =
      bioLabels.indices.forall(isValid(bioLabels, _))

  // Only INSIDEs can be invalid and they are made valid by
  // converting them into a BEGIN.
  def toBegin(bioLabel: String): String =
      BEGIN + bioLabel.drop(INSIDE.length)

  // Note that this patches the array in place!
  def patch(bioLabels: Array[String]): Array[String] = {
    bioLabels.indices.foreach { index =>
      if (!isValid(bioLabels, index))
        bioLabels(index) = toBegin(bioLabels(index))
    }
    bioLabels
  }
}
