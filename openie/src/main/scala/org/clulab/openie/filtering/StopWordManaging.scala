package org.clulab.openie.filtering

trait StopWordManaging {
  /** Whether or not the entity is considered `stop` content. */
  def isStopNER(entity: String): Boolean
  def isStopOrTransparentWord(word: String): Boolean
  def isStopWord(word: String): Boolean = isStopOrTransparentWord(word)
}

