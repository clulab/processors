package org.clulab.dynet
import com.typesafe.config.Config
import edu.cmu.dynet.{Dim, Expression, FloatVector, LookupParameter, ParameterCollection}
import org.clulab.embeddings.{WordEmbeddingMap, WordEmbeddingMapPool}
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.utils.ConfigWithDefaults
import org.clulab.utils.StringUtils

class ConstEmbeddingsGlove

/**
 * Implements the ConstEmbeddings as a thin wrapper around WordEmbeddingMap
 *   with additional functionality to produce embeddings as DyNet Expressions
 */
object ConstEmbeddingsGlove {
  val logger:Logger = LoggerFactory.getLogger(classOf[ConstEmbeddingsGlove])

  // This is not marked private for debugging purposes
  var SINGLETON_WORD_EMBEDDING_MAP: Option[WordEmbeddingMap] = None

  // make sure the singleton is loaded
  load()

  def dim: Int = {
    // this does not need to be synchronized, but the singleton must be created before
    assert(SINGLETON_WORD_EMBEDDING_MAP.isDefined)
    SINGLETON_WORD_EMBEDDING_MAP.get.dim
  }

  def mkConstLookupParams(words: IndexedSeq[String]): (ParameterCollection, LookupParameter) = {
    // This does not need to be synchronized, but the singleton must be created before.
    assert(SINGLETON_WORD_EMBEDDING_MAP.isDefined)

    val parameters = new ParameterCollection()
    val embeddings = SINGLETON_WORD_EMBEDDING_MAP.get
    val dim = embeddings.dim
    val wordLookupParameters = parameters.addLookupParameters(words.length, Dim(dim))
    // This single object will be reused to shuffle data to the wordLookupParameters.
    val floatVector = new FloatVector(dim)

    words
        .map(embeddings.getOrElseUnknown)
        .zipWithIndex
        .foreach { case (embedding, wordIndex) =>
          embedding.zipWithIndex.foreach { case (float, floatIndex) =>
            floatVector.update(floatIndex, float)
          }
          wordLookupParameters.initialize(wordIndex, floatVector)
        }

    (parameters, wordLookupParameters)
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
