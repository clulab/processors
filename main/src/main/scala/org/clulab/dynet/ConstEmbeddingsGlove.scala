package org.clulab.dynet
import edu.cmu.dynet.{Dim, Expression, FloatVector}
import org.clulab.embeddings.WordEmbeddingMapPool
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.utils.ConfigWithDefaults
import org.clulab.utils.StringUtils

/**
 * Implements the ConstEmbeddings as a thin wrapper around WordEmbeddingMap
 *   with additional functionality to produce embeddings as DyNet Expressions
 */
class ConstEmbeddingsGlove(matrixResourceName: String) extends ConstEmbeddings {

  val wordVectors = {
    // This is really meant to be a resource location, but we'll take a file if it's there.
    val name = StringUtils.afterLast(matrixResourceName, '/', all = true, keep = false)
    val location = StringUtils.beforeLast(matrixResourceName, '/', all = false, keep = true)

    WordEmbeddingMapPool.getOrElseCreate(name, compact = true, location, location)
  }
  // CompactWordEmbeddingMap(matrixResourceName, resource = isResource, cached = false)

  override def dim: Int = wordVectors.dim

  override def mkEmbedding(word:String): Expression = {
    val vector = wordVectors.getOrElseUnknown(word)
    Expression.input(Dim(dim), new FloatVector(vector))
  }

}

object ConstEmbeddingsGlove {
  val logger:Logger = LoggerFactory.getLogger(classOf[ConstEmbeddingsGlove])

  private def UNK:String = "" // empty string for UNK

  def apply(configName:String = "org/clulab/glove.conf"): ConstEmbeddingsGlove = {
    val config = ConfigWithDefaults(configName)
    val matrixResourceName = config.getArgString("glove.matrixResourceName", None)
//    val isResource = config.getArgBoolean("glove.isResource", Some(true))
    new ConstEmbeddingsGlove(matrixResourceName)
  }
}
