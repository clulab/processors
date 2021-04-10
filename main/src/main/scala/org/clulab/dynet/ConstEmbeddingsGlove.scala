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

  private val unknownIdx = w2i(wordEmbeddingMap.unknownKey)

  override def mkEmbedding(word: String): Expression = {
    val idx = w2i.getOrElse(word, unknownIdx)
    Expression.constLookup(lookupParameters, idx)
  }

  private def mkLookupParams(): (LookupParameter, Map[String, Int]) = {
    logger.debug("Started converting word embeddings into DyNet LookupParameters...")
    // note: it is important that the key for the unknown token be included in this
    val keys = wordEmbeddingMap.keys
    val w2i = keys.toList.sorted.zipWithIndex.toMap

    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(dim))

    for(word <- keys) {
      try {
        wordLookupParameters.initialize(w2i(word), new FloatVector(wordEmbeddingMap.get(word).get))
      } catch {
        case e: NoSuchElementException =>
          println(s"Could not find vector for key: $word")
          e.printStackTrace()
          throw e
      }
    }
    logger.debug(s"Completed the creation of LookupParameters of dimension $dim for ${w2i.size} words.")

    (wordLookupParameters, w2i)
  }
}

object ConstEmbeddingsGlove {
  val logger:Logger = LoggerFactory.getLogger(classOf[ConstEmbeddingsGlove])

  def apply(configName: String = "org/clulab/glove.conf"): ConstEmbeddingsGlove =
      apply(ConfigWithDefaults(configName))

  def apply(conf: Config): ConstEmbeddingsGlove = apply(ConfigWithDefaults(conf))

  def apply(config: ConfigWithDefaults): ConstEmbeddingsGlove = {
    val matrixResourceName = config.getArgString("glove.matrixResourceName", None)
    val wordEmbeddingMap = {
      // This is really meant to be a resource location, but we'll take a file if it's there.
      // val isResource = config.getArgBoolean("glove.isResource", Some(true))
      val name = StringUtils.afterLast(matrixResourceName, '/', all = true, keep = false)
      val location = StringUtils.beforeLast(matrixResourceName, '/', all = false, keep = true)

      WordEmbeddingMapPool.getOrElseCreate(name, compact = true, location, location)
      // CompactWordEmbeddingMap(matrixResourceName, resource = isResource, cached = false)
    }

    new ConstEmbeddingsGlove(wordEmbeddingMap)
  }
}
