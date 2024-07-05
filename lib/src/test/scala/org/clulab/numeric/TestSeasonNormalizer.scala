package org.clulab.numeric

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.utils.Test

class TestSeasonNormalizer extends Test {
  val autumnText = "When the leaves changed color in autumn 2017 they were the prettiest ever."
  val seasonText = "When the leaves changed color in rainy season 2017 they were the prettiest ever."

  val bDateRange = "B-DATE-RANGE"
  val iDateRange = "I-DATE-RANGE"

  val fallDateRange = "2017-09-22 -- 2017-12-21"
  val seasonDateRange = "2017-06-XX -- 2017-10-XX"

  def mkEntitiesAndNorms(processor: BalaurProcessor, text: String): (Array[String], Array[String]) = {
    val document = processor.annotate(text)
    val mentions = processor.extractNumericEntityMentions(document)

    setLabelsAndNorms(document, mentions)
    (document.sentences.head.entities.get, document.sentences.head.norms.get)
  }

  behavior of "Default seasonal BalaurProcessor"

  it should "find autumn but not rainy season" in {
    val processor = new BalaurProcessor()

    val (autumnEntities, autumnNorms) = mkEntitiesAndNorms(processor, autumnText)
    autumnEntities should contain (bDateRange)
    autumnEntities should contain (iDateRange)
    autumnNorms should contain (fallDateRange)
    autumnNorms shouldNot contain (seasonDateRange)

    val (seasonEntities, seasonNorms) = mkEntitiesAndNorms(processor, seasonText)
    seasonEntities shouldNot contain (bDateRange)
    seasonEntities shouldNot contain (iDateRange)
    seasonNorms shouldNot contain (fallDateRange)
    seasonNorms shouldNot contain (seasonDateRange)

  }

  behavior of "Custom seasonal BalaurProcessor"

  it should "find rainy season but not autumn" in {
    // The file name should remain SEASONS, but it can be put in a different location.
    val processor = new BalaurProcessor(seasonPathOpt = Some("/org/clulab/numeric/custom/SEASON.tsv"))

    val (autumnEntities, autumnNorms) = mkEntitiesAndNorms(processor, autumnText)
    autumnEntities shouldNot contain (bDateRange)
    autumnEntities shouldNot contain (iDateRange)
    autumnNorms shouldNot contain (fallDateRange)
    autumnNorms shouldNot contain (seasonDateRange)

    val (seasonEntities, seasonNorms) = mkEntitiesAndNorms(processor, seasonText)
    seasonEntities should contain (bDateRange)
    seasonEntities should contain (iDateRange)
    seasonNorms shouldNot contain (fallDateRange)
    seasonNorms should contain (seasonDateRange)

  }

}