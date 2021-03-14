package org.clulab.dynet
import edu.cmu.dynet.{Dim, Expression, FloatVector}
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
  val dynetDim: Dim = Dim(wordEmbeddingMap.dim)

  override def dim: Int = wordEmbeddingMap.dim

  override def mkEmbedding(word: String): Expression = {
    val embeddingSeq: IndexedSeq[Float] = wordEmbeddingMap.getOrElseUnknown(word)
    val floatVector = new FloatVector(embeddingSeq.length)
    // Populate this manually to avoid floatToFloat and asJavaCollection
    var i = 0
    while (i < embeddingSeq.length) {
      floatVector.update(i, embeddingSeq(i))
      i += 1
    }
    Expression.input(dynetDim, floatVector)
  }
}

object ConstEmbeddingsGlove {
  val logger:Logger = LoggerFactory.getLogger(classOf[ConstEmbeddingsGlove])

  def apply(configName: String = "org/clulab/glove.conf"): ConstEmbeddingsGlove = {
    val config = ConfigWithDefaults(configName)
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
