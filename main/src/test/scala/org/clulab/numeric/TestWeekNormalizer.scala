package org.clulab.numeric

import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.Test

class TestWeekNormalizer extends Test {
  val firstWeek = "We planted corn the first week of April."
//  val lastWeek = "We planted beans the last week of May."

  val bDateRange = "B-DATE-RANGE"
  val iDateRange = "I-DATE-RANGE"

  val firstWeekAprilRange = "XXXX-04-01 -- XXXX-04-07"
//  val lastWeekMayRange = "XXXX-05-25 -- XXXX-05-31"

  def mkEntitiesAndNorms(processor: CluProcessor, text: String): (Array[String], Array[String]) = {
    val document = processor.annotate(text)
    val mentions = processor.numericEntityRecognizer.extractFrom(document)

    setLabelsAndNorms(document, mentions)
    (document.sentences.head.entities.get, document.sentences.head.norms.get)
  }

  behavior of "WeekCluProcessor"

  it should "find first week of April" in {
    val processor = new CluProcessor()

    val (firstWeekEntities, firstWeekNorms) = mkEntitiesAndNorms(processor, firstWeek)
    firstWeekEntities should contain (bDateRange)
    firstWeekEntities should contain (iDateRange)
    firstWeekNorms should contain (firstWeekAprilRange)
//    firstWeekNorms shouldNot contain (lastWeekMayRange)

//    val (lastWeekEntities, lastWeekNorms) = mkEntitiesAndNorms(processor, lastWeek)
//    lastWeekEntities should contain (bDateRange)
//    lastWeekEntities should contain (iDateRange)
//    lastWeekNorms should contain (lastWeekMayRange)
//    lastWeekNorms shouldNot contain (firstWeekAprilRange)
  }
//
//  behavior of "Custom SeasonalCluProcessor"
//
//  it should "find rainy season but not autumn" in {
//    // The file name should remain SEASONS, but it can be put in a different location.
//    val processor = new CluProcessor(seasonPathOpt = Some("/org/clulab/numeric/custom/SEASON.tsv"))
//
//    val (autumnEntities, autumnNorms) = mkEntitiesAndNorms(processor, autumnText)
//    autumnEntities shouldNot contain (bDateRange)
//    autumnEntities shouldNot contain (iDateRange)
//    autumnNorms shouldNot contain (fallDateRange)
//    autumnNorms shouldNot contain (seasonDateRange)
//
//    val (seasonEntities, seasonNorms) = mkEntitiesAndNorms(processor, seasonText)
//    seasonEntities should contain (bDateRange)
//    seasonEntities should contain (iDateRange)
//    seasonNorms shouldNot contain (fallDateRange)
//    seasonNorms should contain (seasonDateRange)
//  }
}
