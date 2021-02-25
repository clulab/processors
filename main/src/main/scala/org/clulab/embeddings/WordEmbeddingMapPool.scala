package org.clulab.embeddings

import scala.collection.mutable

/** Manages a pool of word embedding maps, so we do not load them more than once */
object WordEmbeddingMapPool {

  case class Key(filename: String, compact: Boolean)

  /** Stores all embedding maps that have been accessed */
  protected val pool = new mutable.HashMap[Key, WordEmbeddingMap]()

  /** Fetches an embedding from the pool if it exists, or creates it otherwise */
  def getOrElseCreate(filename: String,
                      resource: Boolean = true,
                      cached: Boolean = false,
                      compact: Boolean = false): WordEmbeddingMap = {

    this.synchronized {
      pool.getOrElseUpdate(Key(filename, compact),
        if (compact) CompactWordEmbeddingMap(filename, resource, cached)
        else ExplicitWordEmbeddingMap(filename, resource, cached)
      )
    }
  }

  /** Removes an embedding map from the pool */
  def remove(filename: String, compact: Boolean = false): Unit = {
    this.synchronized {
      pool.remove(Key(filename, compact))
    }
  }
}
