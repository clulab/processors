package org.clulab.numeric

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.dynet.Metal
import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.tokenizer.Lemmatizer
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.sequences.LexiconNER
import org.clulab.utils.Test

class TestSeasonNormalizer extends Test {
  Utils.initializeDyNet()

  val autumnText = "When the leaves changed color in autumn 2017 they were the prettiest ever."
  val seasonText = "When the leaves changed color in season 2017 they were the prettiest ever."

  val bDateRange = "B-DATE-RANGE"
  val iDateRange = "I-DATE-RANGE"

  val dateRange = "2017-09-22 -- 2017-12-21"

  def mkEntitiesAndNorms(processor: SeasonalCluProcessor, text: String): (Array[String], Array[String]) = {
    val document = processor.annotate(text)
    val mentions = processor.numericEntityRecognizer.extractFrom(document)

    setLabelsAndNorms(document, mentions)
    (document.sentences.head.entities.get, document.sentences.head.norms.get)
  }

  behavior of "Default SeasonalCluProcessor"

  ignore should "find autumn but not season" in {
    val processor = new SeasonalCluProcessor()

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
    val processor = new SeasonalCluProcessor(seasonPathOpt = Some("/org/clulab/numeric/CUSTOM_SEASON.tsv"))

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

class SeasonalCluProcessor protected(
  config: Config,
  optionalNER: Option[LexiconNER],
  numericEntityRecognizer: NumericEntityRecognizer, // This differs from superclass.
  internStringsOpt: Option[Boolean],
  localTokenizerOpt: Option[Tokenizer],
  lemmatizerOpt: Option[Lemmatizer],
  mtlPosChunkSrlpOpt: Option[Metal],
  mtlNerOpt: Option[Metal],
  mtlSrlaOpt: Option[Metal],
  mtlDepsOpt: Option[Metal]
) extends CluProcessor(
  config, optionalNER, Some(numericEntityRecognizer), internStringsOpt, localTokenizerOpt, lemmatizerOpt,
  mtlPosChunkSrlpOpt, mtlNerOpt, mtlSrlaOpt, mtlDepsOpt
) {

  def this(
    config: Config = ConfigFactory.load("cluprocessor"),
    optionalNER: Option[LexiconNER] = None,
    seasonPathOpt: Option[String] = None
  ) = this(config, optionalNER, SeasonalCluProcessor.newNumericEntityRecognizer(seasonPathOpt), None, None, None, None, None, None, None)
}

object SeasonalCluProcessor {

  def newNumericEntityRecognizer(seasonPathOpt: Option[String]): NumericEntityRecognizer = {
    val seasonPath = seasonPathOpt.getOrElse(NumericEntityRecognizer.seasonPath)
    val numericEntityRecognizer = NumericEntityRecognizer(seasonPath)

    numericEntityRecognizer
  }
}
