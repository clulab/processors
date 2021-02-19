package org.clulab.embeddings

import scala.collection.mutable

/** Manages a pool of word embedding maps, so we do not load them more than once */
object WordEmbeddingMapPool {
  /** Stores all embedding maps that have been accessed */
  val pool = new mutable.HashMap[String, WordEmbeddingMap]()

  private def mkKey(filename: String, compact: Boolean): String = {
    s"$filename/$compact"
  }

  /** Fetches an embedding from the pool if it exists, or creates it otherwise */
  def getOrElseCreate(filename: String, resource: Boolean = true, cached: Boolean = false, compact: Boolean = false): Unit = {
    this.synchronized {
      pool.getOrElseUpdate(mkKey(filename, compact),
        if(compact) CompactWordEmbeddingMap(filename, resource, cached)
        else ExplicitWordEmbeddingMap(filename, resource, cached)
      )
    }
  }

  /** Removes an embedding map from the pool */
  def delete(filename: String, compact: Boolean = false): Unit = {
    this.synchronized {
      pool.remove(mkKey(filename, compact))
    }
  }
}
