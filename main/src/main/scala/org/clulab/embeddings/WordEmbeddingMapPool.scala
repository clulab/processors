package org.clulab.embeddings

import org.clulab.scala.Using._
import org.clulab.utils.InputStreamer
import org.clulab.utils.InputStreamer.StreamResult
import org.clulab.utils.NamedFuture

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

/** Manages a pool of word embedding maps, so we do not load them more than once */
object WordEmbeddingMapPool {

  case class Key(name: String, compact: Boolean)

  protected val inputStreamer = new InputStreamer()
  protected val maxWaitTime = Duration.Inf
  protected val enabled = true

  /** Stores all embedding maps that have been accessed */
  protected val pool = new mutable.HashMap[Key, Future[WordEmbeddingMap]]()

  // Check whether it is by chance there already before something creates its own.
  def get(name: String, compact: Boolean = false): Option[WordEmbeddingMap] = {
    val wordEmbeddingMapFutureOpt = this.synchronized {
       pool.get(Key(name, compact))
    }
    val wordEmbeddingMapOpt = wordEmbeddingMapFutureOpt.map(Await.result(_, maxWaitTime))

    wordEmbeddingMapOpt
  }

  /** Fetches an embedding from the pool if it exists, or creates it otherwise */
  def getOrElseCreate(name: String, compact: Boolean = false, fileLocation: String = "", resourceLocation: String = ""): WordEmbeddingMap = {
    // Using the global execution context may be a bad strategy.
    import scala.concurrent.ExecutionContext.Implicits.global

    val wordEmbeddingMapFuture =
      if (enabled)
        this.synchronized {
          // Access the shared pool inside the synchronized section.
          pool.getOrElseUpdate(
            Key(name, compact),
            NamedFuture("enabled WordEmbeddingMapPool.loadEmbedding") {
              loadEmbedding(name, fileLocation, resourceLocation, compact = compact)
            }
          )
        }
      else
        NamedFuture("disabled WordEmbeddingMapPool.loadEmbedding") {
          loadEmbedding(name, fileLocation, resourceLocation, compact = compact)
        }
    // Wait for the result outside the synchronized section.
    Await.result(wordEmbeddingMapFuture, maxWaitTime)
  }

  /** Removes an embedding map from the pool */
  def remove(filename: String, compact: Boolean = false): Unit = {
    this.synchronized {
      pool.remove(Key(filename, compact))
    }
  }

  def clear(): Unit = {
    this.synchronized {
      pool.clear()
    }
  }

  def loadEmbedding(name: String, fileLocation: String, resourceLocation: String, compact: Boolean): WordEmbeddingMap = {
    val StreamResult(inputStream, _, format) = inputStreamer.stream(name, fileLocation, resourceLocation)
        .getOrElse(throw new RuntimeException(s"WordEmbeddingMap $name could not be opened."))
    val wordEmbeddingMap = Using.resource(inputStream) { inputStream =>
      val binary = format == InputStreamer.Format.Bin

      if (compact) CompactWordEmbeddingMap(inputStream, binary)
      else ExplicitWordEmbeddingMap(inputStream, binary)
    }

    wordEmbeddingMap
  }
}
