package org.clulab.numeric

import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.Test

class TestWeekNormalizer extends Test {
  val firstTwoWeeks = "We planted corn the first two weeks of April."
  val secondWeek = "We planted beans the second week of May."
  val lastWeek = "We planted beans in the last week of June."
  val lastTwoWeeks = "We planted beans in the last two weeks of February."

  val bDateRange = "B-DATE-RANGE"
  val iDateRange = "I-DATE-RANGE"

  val firstTwoWeeksAprilRange = "XXXX-04-01 -- XXXX-04-14"
  val secondWeekMayRange = "XXXX-05-08 -- XXXX-05-14"
  val lastWeekJuneRange = "XXXX-06-24 -- XXXX-06-30"
  val lastTwoWeeksFebRange = "XXXX-02-15 -- XXXX-02-28"

  def mkEntitiesAndNorms(processor: CluProcessor, text: String): (Array[String], Array[String]) = {
    val document = processor.annotate(text)
    val mentions = processor.numericEntityRecognizer.extractFrom(document)

    setLabelsAndNorms(document, mentions)
    (document.sentences.head.entities.get, document.sentences.head.norms.get)
  }

  behavior of "WeekCluProcessor"

  it should "find first two weeks of April" in {
    val processor = new CluProcessor()

    val (firstTwoWeeksEntities, firstTwoWeeksNorms) = mkEntitiesAndNorms(processor, firstTwoWeeks)
    firstTwoWeeksEntities should contain(bDateRange)
    firstTwoWeeksEntities should contain(iDateRange)
    firstTwoWeeksNorms should contain(firstTwoWeeksAprilRange)
    firstTwoWeeksNorms shouldNot contain(secondWeekMayRange)
  }

  it should "find second week of May" in {
    val processor = new CluProcessor()

    val (secondWeekEntities, secondWeekNorms) = mkEntitiesAndNorms(processor, secondWeek)
    secondWeekEntities should contain (bDateRange)
    secondWeekEntities should contain (iDateRange)
    secondWeekNorms should contain (secondWeekMayRange)
    secondWeekNorms shouldNot contain (firstTwoWeeksAprilRange)
  }

  it should "find last week of June" in {
    val processor = new CluProcessor()

    val (lastWeekEntities, lastWeekNorms) = mkEntitiesAndNorms(processor, lastWeek)
    lastWeekEntities should contain(bDateRange)
    lastWeekEntities should contain(iDateRange)
    lastWeekNorms should contain(lastWeekJuneRange)
    lastWeekNorms shouldNot contain(secondWeekMayRange)
  }

  it should "find last two weeks of February" in {
    val processor = new CluProcessor()

    val (lastTwoWeeksEntities, lastTwoWeeksNorms) = mkEntitiesAndNorms(processor, lastTwoWeeks)
    lastTwoWeeksEntities should contain (bDateRange)
    lastTwoWeeksEntities should contain (iDateRange)
    lastTwoWeeksNorms should contain (lastTwoWeeksFebRange)
    lastTwoWeeksNorms shouldNot contain (firstTwoWeeksAprilRange)
  }

}
