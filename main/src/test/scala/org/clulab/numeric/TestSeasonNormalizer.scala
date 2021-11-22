package org.clulab.numeric

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.Test

class TestSeasonNormalizer extends Test {
  Utils.initializeDyNet()

  val autumnText = "When the leaves changed color in autumn 2017 they were the prettiest ever."
  val seasonText = "When the leaves changed color in season 2017 they were the prettiest ever."

  val bDateRange = "B-DATE-RANGE"
  val iDateRange = "I-DATE-RANGE"

  val dateRange = "2017-09-22 -- 2017-12-21"

  def mkEntitiesAndNorms(processor: CluProcessor, text: String): (Array[String], Array[String]) = {
    val document = processor.annotate(text)
    val mentions = processor.numericEntityRecognizer.extractFrom(document)

    setLabelsAndNorms(document, mentions)
    (document.sentences.head.entities.get, document.sentences.head.norms.get)
  }

  behavior of "Default SeasonalCluProcessor"

  it should "find autumn but not season" in {
    val processor = new CluProcessor()

    val (autumnEntities, autumnNorms) = mkEntitiesAndNorms(processor, autumnText)
    autumnEntities should contain (bDateRange)
    autumnEntities should contain (iDateRange)
    autumnNorms should contain (dateRange)

    val (seasonEntities, seasonNorms) = mkEntitiesAndNorms(processor, seasonText)
    seasonEntities shouldNot contain (bDateRange)
    seasonEntities shouldNot contain (iDateRange)
    seasonNorms shouldNot contain (dateRange)
  }

  behavior of "Custom SeasonalCluProcessor"

  it should "find season but not autumn" in {
    val processor = new CluProcessor(seasonPathOpt = Some("/org/clulab/numeric/CUSTOM_SEASON.tsv"))

    val (autumnEntities, autumnNorms) = mkEntitiesAndNorms(processor, autumnText)
    autumnEntities shouldNot contain (bDateRange)
    autumnEntities shouldNot contain (iDateRange)
    autumnNorms shouldNot contain (dateRange)

    val (seasonEntities, seasonNorms) = mkEntitiesAndNorms(processor, seasonText)
    seasonEntities should contain (bDateRange)
    seasonEntities should contain (iDateRange)
    seasonNorms should contain (dateRange)
  }
}
