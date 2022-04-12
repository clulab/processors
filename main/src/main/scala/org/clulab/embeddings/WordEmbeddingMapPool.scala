package org.clulab.embeddings

import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.InputStreamer
import org.clulab.utils.InputStreamer.StreamResult

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

/** Manages a pool of word embedding maps, so we do not load them more than once */
object WordEmbeddingMapPool {
  import scala.concurrent.ExecutionContext.Implicits.global

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
    val wordEmbeddingMapFuture =
      if (enabled)
        this.synchronized {
          // Access the shared pool inside the synchronized section.
          pool.getOrElseUpdate(
            Key(name, compact),
            Future {
              loadEmbedding(name, fileLocation, resourceLocation, compact = compact)
            }
          )
        }
      else
        Future {
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
    val wordEmbeddingMap = inputStream.autoClose { inputStream =>
      val binary = format == InputStreamer.Format.Bin

      if (compact) CompactWordEmbeddingMap(inputStream, binary)
      else ExplicitWordEmbeddingMap(inputStream, binary)
    }

    wordEmbeddingMap
  }
}
