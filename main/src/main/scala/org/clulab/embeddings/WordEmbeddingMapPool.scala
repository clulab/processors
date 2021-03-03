package org.clulab.embeddings

import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer

import java.io.FileInputStream
import java.io.InputStream
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

  def getFileAsStream(name: String): FileInputStream = new FileInputStream(name)

  def getResourceAsStream(name: String): InputStream = {
//    val result = {
//      // This works for ./ resources, not / resources, and doesn't therefore work for library jars like glove.
//      val classLoader = this.getClass.getClassLoader
//      classLoader.getResourceAsStream(name)
//    }
    val result = this.getClass.getResourceAsStream(name)

    Option(result).getOrElse(throw new RuntimeException(s"Resource $name not found."))
  }

  def getSource(name: String): Option[(InputStream, Location.Location, Format.Format)] = {
    val binName = name + binExtension
    val txtName = name + txtExtension

    val result = Failure(null)
        .orElse(Try(getFileAsStream(binName),     Location.File,     Format.Bin))
        .orElse(Try(getFileAsStream(txtName),     Location.File,     Format.Txt))
        .orElse(Try(getResourceAsStream(binName), Location.Resource, Format.Bin))
        .orElse(Try(getResourceAsStream(txtName), Location.Resource, Format.Txt))
        .toOption

    result
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
    val (inputStream, _, format) = getSource(name)
        .getOrElse(throw new RuntimeException(s"WordEmbeddingMap $name could not be opened."))
    val wordEmbeddingMap = inputStream.autoClose { inputStream =>
      val binary = format == Format.Bin

      if (compact) CompactWordEmbeddingMap(inputStream, binary)
      else ExplicitWordEmbeddingMap(inputStream, binary)
    }

    wordEmbeddingMap
  }
}
