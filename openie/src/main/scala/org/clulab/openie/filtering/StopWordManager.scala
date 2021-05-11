package org.clulab.openie.filtering

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.utils.FileUtils

class StopWordManager(stopWordsPath: String, transparentPath: String, tagSet: TagSet) extends StopWordManaging {
  protected val stopWords: Set[String] = FileUtils.getCommentedTextSetFromResource(stopWordsPath)
  protected val transparentWords: Set[String] = FileUtils.getCommentedTextSetFromResource(transparentPath)
  protected val bothWords: Set[String] = stopWords ++ transparentWords

  def isStopNER(entities: Option[Array[String]], i: Int): Boolean = entities.exists(e => isStopNER(e(i)))
  def isStopNER(entity: String): Boolean = StopWordManager.STOP_NER.contains(entity)

  def isStopOrTransparentWord(stopword: String): Boolean = bothWords.contains(stopword)

  override def isStopWord(word: String): Boolean = stopWords.contains(word)
  def isTransparentWord(word: String): Boolean = transparentWords.contains(word)

  def hasNonStopContent(s: Sentence, span: Interval): Boolean = {
    // method only meaningful if lemmas and tags are in place
    require(s.lemmas.nonEmpty && s.tags.nonEmpty)

    val lemmas = s.lemmas.get.slice(span.start, span.end)
    val tags = s.tags.get.slice(span.start, span.end)
    val entities = s.entities.map(_.slice(span.start, span.end))

    // There should be at least one noun
    if (!tags.exists(tagSet.isAnyNoun)) return false
    // This above can be combined with those below

    // There should be at least one word which:
    lemmas.indices.exists { i =>
      // has more than one character
      lemmas(i).length > 1 &&
        // has a content POS tag
        tagSet.isStopwordContent(tags(i)) &&
        // isn't a stopword
        !isStopOrTransparentWord(lemmas(i)) &&
        // and isn't a stop NER, if NER available
       !isStopNER(entities, i)
    }
  }



}

object StopWordManager {
  // maybe use this to get missed Locations/Dates/etc?; not sure if necessary anymore?
  //  val STOP_NER: Set[String] = Set("DURATION", "MONEY", "NUMBER", "ORDINAL", "ORGANIZATION", "PERCENT", "SET")
  val STOP_NER: Set[String] = Set("DATE", "DURATION", "LOCATION", "MISC", "MONEY", "NUMBER", "ORDINAL", "ORGANIZATION", "PERSON", "PLACE", "SET", "TIME")

  def apply(stopwordsPath: String, transparentPath: String, tagSet: TagSet) =
    new StopWordManager(stopwordsPath, transparentPath, tagSet)

  def fromConfig(config: Config = ConfigFactory.load(), tagSet: TagSet = new EnglishTagSet) = {
    val stopWordsPath: String = config.getString("openie.stops")
    val transparentPath: String = config.getString("openie.transparent")
    apply(stopWordsPath, transparentPath, tagSet)
  }
}

