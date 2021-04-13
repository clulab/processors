package org.clulab.dynet
import com.typesafe.config.Config
import edu.cmu.dynet.{Dim, Expression, FloatVector, LookupParameter, ParameterCollection}
import org.clulab.embeddings.WordEmbeddingMap
import org.clulab.embeddings.WordEmbeddingMapPool
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.utils.ConfigWithDefaults
import org.clulab.utils.StringUtils

import ConstEmbeddingsGlove.logger

/**
 * Implements the ConstEmbeddings as a thin wrapper around WordEmbeddingMap
 *   with additional functionality to produce embeddings as DyNet Expressions
 */
class ConstEmbeddingsGlove(wordEmbeddingMap: WordEmbeddingMap) extends ConstEmbeddings {

  /** These parameters are never updated, so we can share this object between models */
  private val parameters = new ParameterCollection()

  private val (lookupParameters, w2i) = mkLookupParams()

  override def dim: Int = wordEmbeddingMap.dim

  override def mkEmbedding(word: String): Expression = {
    val idx = w2i.getOrElse(word, 0) // 0 is unknown
    Expression.constLookup(lookupParameters, idx)
  }

  protected def shift(range: Range, n: Int): Range = {
    require(!range.isInclusive)
    Range(range.start + n, range.end + n, range.step)
  }

  private def mkLookupParams(): (LookupParameter, Map[String, Int]) = {
    logger.debug("Started converting word embeddings into DyNet LookupParameters...")
    val keys = wordEmbeddingMap.keys.toArray.sorted
    // add 1 to the index because 0 is reserved for unknown
    val w2i = keys.zip(shift(keys.indices, 1)).toMap
    val wordLookupParameters = parameters.addLookupParameters(keys.length + 1, Dim(dim))

    // index 0 reserved for the embedding of the unknown token
    wordLookupParameters.initialize(0, new FloatVector(wordEmbeddingMap.unknownEmbedding))
    for((word, index) <- w2i)
      wordLookupParameters.initialize(index, new FloatVector(wordEmbeddingMap.getOrElseUnknown(word)))
    logger.debug(s"Completed the creation of LookupParameters of dimension $dim for ${w2i.size} words.")
    (wordLookupParameters, w2i)
  }
}

object ConstEmbeddingsGlove {
  val logger:Logger = LoggerFactory.getLogger(classOf[ConstEmbeddingsGlove])

  def apply(configName: String = "org/clulab/glove.conf"): ConstEmbeddingsGlove =
      apply(ConfigWithDefaults(configName))

  def apply(conf: Config): ConstEmbeddingsGlove = apply(ConfigWithDefaults(conf))

  // This is not marked private for debugging purposes.
  var SINGLETON: Option[ConstEmbeddingsGlove] = None

  def apply(config: ConfigWithDefaults): ConstEmbeddingsGlove = {

    this.synchronized { // synchronized so we don't create multiple SINGLETON objects
      if (SINGLETON.isEmpty) {
        val matrixResourceName = config.getArgString("glove.matrixResourceName", None)

        val wordEmbeddingMap = {
          // This is really meant to be a resource location, but we'll take a file if it's there.
          // val isResource = config.getArgBoolean("glove.isResource", Some(true))
          val name = StringUtils.afterLast(matrixResourceName, '/', all = true, keep = false)
          val location = StringUtils.beforeLast(matrixResourceName, '/', all = false, keep = true)

          WordEmbeddingMapPool.getOrElseCreate(name, compact = true, location, location)
          // CompactWordEmbeddingMap(matrixResourceName, resource = isResource, cached = false)
        }

        SINGLETON = Some(new ConstEmbeddingsGlove(wordEmbeddingMap))
      }
    }

    SINGLETON.get
  }
}
