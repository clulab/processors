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


  /** Constructs ConstEmbeddingParameters from a *set* of words, which may come from a Sentence or a Document */
  private def mkConstLookupParams(words: Set[String]): ConstEmbeddingParameters = {
    // this does not need to be synchronized, but the singleton must be created before
    assert(SINGLETON_WORD_EMBEDDING_MAP.isDefined)

    val embeddings = SINGLETON_WORD_EMBEDDING_MAP.get
    val parameters = new ParameterCollection()
    val dim = embeddings.dim

    var knownCount = 0
    val w2i = new mutable.HashMap[String, Int]()
    for(word <- words) {
      if(! embeddings.isOutOfVocabulary(word)) {
        knownCount += 1
        w2i += word -> knownCount // 0 is reserved for unknown
      }
    }

    val wordLookupParameters = parameters.addLookupParameters(knownCount + 1, Dim(dim)) // one extra position for unknown
    wordLookupParameters.initialize(0, embeddings.unknownEmbedding) // 0 is reserved for unknown
    for(word <- w2i.keySet) {
      val index = w2i(word)
      val vec = embeddings.get(word)
      assert(vec.isDefined) // we checked above, so this should be true
      wordLookupParameters.initialize(index, new FloatVector(vec.get))
    }

    ConstEmbeddingParameters(parameters, wordLookupParameters, w2i.toMap)
  }

  def load(configName: String = "org/clulab/glove.conf") {
    load(ConfigWithDefaults(configName))
  }

  def load(conf: Config) {
    load(ConfigWithDefaults(conf))
  }

  def load(config: ConfigWithDefaults) {
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
