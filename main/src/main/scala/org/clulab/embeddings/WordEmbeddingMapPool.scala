package org.clulab.embeddings

import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import scala.collection.mutable
import scala.io.BufferedSource
import scala.util.Failure
import scala.util.Try

/** Manages a pool of word embedding maps, so we do not load them more than once */
object WordEmbeddingMapPool {
  val binExtension = ".bin"
  val txtExtension = ".txt"

  object Location extends Enumeration {
    type Location = Value
    val File, Resource = Value
  }

  object Format extends Enumeration {
    type Format = Value
    val Txt, Bin = Value
  }

  def getSource(name: String): Option[(BufferedSource, Location.Location, Format.Format)] = {
    val binName = name + binExtension
    val txtName = name + txtExtension

    Failure(null)
        .orElse(Try(Sourcer.sourceFromFile(binName),     Location.File,     Format.Bin))
        .orElse(Try(Sourcer.sourceFromFile(txtName),     Location.File,     Format.Txt))
        .orElse(Try(Sourcer.sourceFromResource(binName), Location.Resource, Format.Bin))
        .orElse(Try(Sourcer.sourceFromResource(txtName), Location.Resource, Format.Txt))
        .toOption
  }

  case class Key(name: String, compact: Boolean)

  /** Stores all embedding maps that have been accessed */
  protected val pool = new mutable.HashMap[Key, WordEmbeddingMap]()

  /** Fetches an embedding from the pool if it exists, or creates it otherwise */
  def getOrElseCreate(name: String, compact: Boolean = false): WordEmbeddingMap = {
    this.synchronized {
      pool.getOrElseUpdate(Key(name, compact), loadEmbedding(name, compact))
    }
  }

  /** Removes an embedding map from the pool */
  def remove(filename: String, compact: Boolean = false): Unit = {
    this.synchronized {
      pool.remove(Key(filename, compact))
    }
  }

  protected def loadEmbedding(name: String, compact: Boolean): WordEmbeddingMap = {
    val (source, _, format) = getSource(name)
        .getOrElse(throw new RuntimeException(s"WordEmbeddingMap $name could not be opened."))
    val wordEmbeddingMap = source.autoClose { source =>
      val binary = format == Format.Bin

      if (compact) CompactWordEmbeddingMap(source, binary)
      else ExplicitWordEmbeddingMap(source, binary)
    }

    wordEmbeddingMap
  }
}
