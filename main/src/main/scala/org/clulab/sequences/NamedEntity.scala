package org.clulab.sequences

// This is definitely not the most efficient as far as number of objects
// created, but there should be a NamedEntity thing to hold and not just
// shadows of it projected onto the BIO notation in an array of strings.
case class NamedEntity(tag: String, range: Range) {

  def fill(bioTags: Array[String]): Unit = {
    bioTags(range.start) = NamedEntity.BEGIN + tag
    if (range.length > 1) {
      val inner = NamedEntity.INSIDE + tag

      range.drop(1).foreach(bioTags(_) = inner)
    }
  }
}

object NamedEntity {
  val BEGIN = "B-"
  val INSIDE = "I-"
  val OUTSIDE = "O"

  def collect(bioTags: IndexedSeq[String]): IndexedSeq[NamedEntity] = {

    def mkNamedEntity(tag: String, begin: Int): NamedEntity = {
      // Start looking for the end one after the begin.
      val end = Range(begin + 1, bioTags.length).find { index =>
        // We're done with the entity if a different one begins or we've landed outside...
        bioTags(index).startsWith(BEGIN) || bioTags(index) == OUTSIDE
      }.getOrElse(bioTags.length) // ...or if we've come to the end.

      NamedEntity(tag, Range(begin, end))
    }

    val tagAndBeginPairs = bioTags.zipWithIndex.filter { case (tag, _) => tag.startsWith(BEGIN) }
    val namedEntities = tagAndBeginPairs.map { case (tag, begin) => mkNamedEntity(tag.drop(BEGIN.length), begin) }

    namedEntities
  }

  def combine(length: Int, genericNamedEntities: Seq[NamedEntity], customNamedEntities: Seq[NamedEntity]): Array[String] = {
    val result = Array.fill(length)(OUTSIDE)
    val customMap = customNamedEntities.map { custom => custom.range -> custom }.toMap

    genericNamedEntities.foreach { genericNamedEntity =>
      // If there is a matching range in the customMap, override the generic one with the
      // custom NamedEntity there.  Otherwise, stick with the generic one.
      val namedEntity = customMap.getOrElse(genericNamedEntity.range, genericNamedEntity)

      namedEntity.fill(result)
    }

    result
  }
}
