package org.clulab.dynet
import com.typesafe.config.Config
import edu.cmu.dynet.{Dim, FloatVector, LookupParameter, ParameterCollection}
import org.clulab.embeddings.{WordEmbeddingMap, WordEmbeddingMapPool}
import org.clulab.processors.{Document, Sentence}
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.utils.ConfigWithDefaults
import org.clulab.utils.StringUtils

import scala.collection.mutable

class ConstEmbeddingsGlove

/** Stores lookup parameters + the map from strings to ids */
case class ConstEmbeddingParameters(collection: ParameterCollection,
                                    lookupParameters: LookupParameter,
                                    w2i: Map[String, Int])

/**
 * Implements the ConstEmbeddings as a thin wrapper around WordEmbeddingMap
 *   with additional functionality to produce embeddings as DyNet Expressions
 */
object ConstEmbeddingsGlove {
  val logger:Logger = LoggerFactory.getLogger(classOf[ConstEmbeddingsGlove])

  // This is not marked private for debugging purposes
  private var SINGLETON_WORD_EMBEDDING_MAP: Option[WordEmbeddingMap] = None

  // make sure the singleton is loaded
  load()

  def dim: Int = {
    // this does not need to be synchronized, but the singleton must be created before
    assert(SINGLETON_WORD_EMBEDDING_MAP.isDefined)
    SINGLETON_WORD_EMBEDDING_MAP.get.dim
  }

  def mkConstLookupParams(words: IndexedSeq[String]): ConstEmbeddingParameters =
    mkConstLookupParams(words.toSet)

  def mkConstLookupParams(sentence: Sentence): ConstEmbeddingParameters =
    mkConstLookupParams(sentence.words.toSet)

  def mkConstLookupParams(doc: Document): ConstEmbeddingParameters = {
    val words = new mutable.HashSet[String]()
    for(s <- doc.sentences) {
      words ++= s.words
    }
    mkConstLookupParams(words.toSet)
  }

  def mkConstLookupParams(words: Set[String], embeddings: WordEmbeddingMap): ConstEmbeddingParameters = {
    val parameters = new ParameterCollection()
    val dim = embeddings.dim
    val w2i = words
      .view
      .filterNot(embeddings.isOutOfVocabulary)
      .zip(1.to(words.size)) // usually 0.until(words.size) but 0 is reserved for unknown
      .toMap[String, Int]
    val wordLookupParameters = parameters.addLookupParameters(w2i.size + 1, Dim(dim)) // one extra position for unknown
    val initializeWordLookupParameters: (Int, IndexedSeq[Float]) => Unit = {
      // Sneak in this single FloatVector to reuse for all transfers of values to the wordLookupParameters.
      val floatVector = new FloatVector(dim)

      (index: Int, embedding: IndexedSeq[Float]) =>
        for ((embedding, index) <- embedding.zipWithIndex)
          floatVector.update(index, embedding)
        wordLookupParameters.initialize(index, floatVector)
    }

    // It is almost always the case that there are unknown words, so the necessity of this is unchecked.
    initializeWordLookupParameters(0, embeddings.unknownEmbedding) // 0 is reserved for unknown
    for ((word, index) <- w2i)
      initializeWordLookupParameters(index, embeddings.get(word).get)
    ConstEmbeddingParameters(parameters, wordLookupParameters, w2i)
  }

  /** Constructs ConstEmbeddingParameters from a *set* of words, which may come from a Sentence or a Document */
  private def mkConstLookupParams(words: Set[String]): ConstEmbeddingParameters = {
    // this does not need to be synchronized, but the singleton must be created before
    assert(SINGLETON_WORD_EMBEDDING_MAP.isDefined)

    val embeddings = SINGLETON_WORD_EMBEDDING_MAP.get

    mkConstLookupParams(words, embeddings)
  }

  def load(configName: String = "org/clulab/glove.conf"): Unit = {
    load(ConfigWithDefaults(configName))
  }

  def load(conf: Config): Unit = {
    load(ConfigWithDefaults(conf))
  }

  def load(config: ConfigWithDefaults): Unit = {
    this.synchronized { // synchronized so we don't create multiple SINGLETON objects
      if (SINGLETON_WORD_EMBEDDING_MAP.isEmpty) {
        val matrixResourceName = config.getArgString("glove.matrixResourceName", None)
        // This is really meant to be a resource location, but we'll take a file if it's there.
        // val isResource = config.getArgBoolean("glove.isResource", Some(true))
        val name = StringUtils.afterLast(matrixResourceName, '/', all = true, keep = false)
        val location = StringUtils.beforeLast(matrixResourceName, '/', all = false, keep = true)

        val embeddings = WordEmbeddingMapPool.getOrElseCreate(name, compact = true, location, location)
        // CompactWordEmbeddingMap(matrixResourceName, resource = isResource, cached = false)

        SINGLETON_WORD_EMBEDDING_MAP = Some(embeddings)
      }
    }
  }

}
