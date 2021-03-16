package org.clulab.dynet
import com.typesafe.config.Config
import edu.cmu.dynet.{Dim, Expression}
import org.clulab.embeddings.WordEmbeddingMap
import org.clulab.embeddings.WordEmbeddingMapPool
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.utils.ConfigWithDefaults
import org.clulab.utils.StringUtils

/**
 * Implements the ConstEmbeddings as a thin wrapper around WordEmbeddingMap
 *   with additional functionality to produce embeddings as DyNet Expressions
 */
class ConstEmbeddingsGlove(wordEmbeddingMap: WordEmbeddingMap) extends ConstEmbeddings {
  protected val dynetDim: Dim = Dim(wordEmbeddingMap.dim)

  override def dim: Int = wordEmbeddingMap.dim

  override def mkEmbedding(word: String): Expression = {
    val vector = wordEmbeddingMap.getOrElseUnknown(word)
    Expression.input(dynetDim, vector)
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
