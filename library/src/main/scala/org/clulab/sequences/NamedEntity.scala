package org.clulab.sequences

import org.clulab.utils.WrappedArraySeq

import scala.collection.mutable

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

  def collect(bioLabels: Seq[String]): Seq[NamedEntity] = {

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

  def combine(bioLabels: Seq[String], genericNamedEntities: Seq[NamedEntity], customNamedEntities: Seq[NamedEntity]): Seq[String] = {
    val bioLabelsArray = bioLabels.toArray
    // Neither named entities sequence can contain overlapping elements within the sequence.
    // At most, there is overlap between sequences.  Use is made of that fact.
    // The NamedEntities never have empty Ranges, so end - 1 is always at least start.
    val outsides = bioLabelsArray.indices.filter(bioLabelsArray(_) == OUTSIDE)
    val validStarts = (genericNamedEntities.map(_.range.start) ++ outsides).toSet
    // The -1 is used to coordinate ends (exclusive) with the OUTSIDE positions (inclusive).
    val validEnds = (genericNamedEntities.map(_.range.end - 1) ++ outsides).toSet
    val validCustomNamedEntities = customNamedEntities.filter { customNamedEntity =>
      validStarts(customNamedEntity.range.start) && validEnds(customNamedEntity.range.end - 1)
    }

    validCustomNamedEntities.foreach { customNamedEntity =>
      customNamedEntity.fill(bioLabelsArray)
    }
    WrappedArraySeq(bioLabelsArray).toImmutableSeq
  }

  // Only INSIDEs can be invalid, and they are made valid by
  // converting them into a BEGIN.
  def toBegin(bioLabel: String): String = BEGIN + bioLabel.drop(INSIDE.length)

  def isValid(bioLabels: Seq[String]): Boolean = bioLabels.indices.forall { index =>
    isValid(bioLabels(index), bioLabels.lift(index - 1))
  }

  def isValid(currBioLabel: String, prevBioLabelOpt: Option[String]): Boolean = {
    !currBioLabel.startsWith(INSIDE) || prevBioLabelOpt.exists { prevBioLabel =>
      prevBioLabel == currBioLabel || prevBioLabel == toBegin(currBioLabel)
    }
  }

  def patch(bioLabels: Seq[String]): Seq[String] = {
    var prevBioLabelOpt = bioLabels.lift(-1)
    val newBioLabels = bioLabels.indices.map { index =>
      val oldBioLabel = bioLabels(index)
      val newBioLabel =
          if (!isValid(oldBioLabel, prevBioLabelOpt)) toBegin(oldBioLabel)
          else oldBioLabel

      prevBioLabelOpt = Some(newBioLabel)
      newBioLabel
    }

    newBioLabels
  }
}
