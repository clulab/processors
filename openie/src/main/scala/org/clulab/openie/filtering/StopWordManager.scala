package org.clulab.openie.filtering

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.openie.utils.{EnglishTagSet, TagSet}
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.utils.FileUtils

import scala.collection.JavaConverters._

class StopWordManager(stopWordsPath: String, transparentPath: String, tagSet: TagSet, stopNER: Set[String]) extends StopWordManaging {
  protected val stopWords: Set[String] = FileUtils.getCommentedTextSetFromResource(stopWordsPath)
  protected val transparentWords: Set[String] = FileUtils.getCommentedTextSetFromResource(transparentPath)
  protected val bothWords: Set[String] = stopWords ++ transparentWords

  def isStopNER(entities: Option[Array[String]], i: Int): Boolean = entities.exists(e => isStopNER(e(i)))
  def isStopNER(entity: String): Boolean = stopNER.contains(entity)

  def isStopOrTransparentWord(stopword: String): Boolean = bothWords.contains(stopword)

  override def isStopWord(word: String): Boolean = stopWords.contains(word)
  def isTransparentWord(word: String): Boolean = transparentWords.contains(word)

  def isContentTag(tag: String): Boolean = {
    // is a noun, verb, or adjective
    tagSet.isAnyNoun(tag) || tagSet.isAnyVerb(tag) || tagSet.isAnyAdjective(tag)
  }

  def hasNonStopContent(lemmas: Seq[String], tags: Seq[String], entities: Option[Array[String]]): Boolean = {
    // There should be at least one noun
    if (!tags.exists(tagSet.isAnyNoun)) return false
    // This above can be combined with those below

    // There should be at least one word which:
    lemmas.indices.exists { i =>
      // has more than one character
      lemmas(i).length > 1 &&
        // has a content POS tag
        isContentTag(tags(i)) &&
        // isn't a stop or transparent word
        !isStopOrTransparentWord(lemmas(i)) &&
        // and isn't a stop NER, if NER available
        !isStopNER(entities, i)
    }
  }

  def hasNonStopContent(s: Sentence, span: Interval): Boolean = {
    // method only meaningful if lemmas and tags are in place
    require(s.lemmas.nonEmpty && s.tags.nonEmpty)

    val lemmas = s.lemmas.get.slice(span.start, span.end)
    val tags = s.tags.get.slice(span.start, span.end)
    val entities = s.entities.map(_.slice(span.start, span.end))

    hasNonStopContent(lemmas, tags, entities)
  }



}

object StopWordManager {

  def apply(stopwordsPath: String, transparentPath: String, tagSet: TagSet, stopNER: Set[String]) =
    new StopWordManager(stopwordsPath, transparentPath, tagSet, stopNER)

  def fromConfig(config: Config = ConfigFactory.load(), tagSet: TagSet = new EnglishTagSet): StopWordManager = {
    val stopWordsPath: String = config.getString("filtering.stops")
    val transparentPath: String = config.getString("filtering.transparent")
    val stopNER: Set[String] = config.getStringList("filtering.stopNER").asScala.toSet
    apply(stopWordsPath, transparentPath, tagSet, stopNER)
  }
}

