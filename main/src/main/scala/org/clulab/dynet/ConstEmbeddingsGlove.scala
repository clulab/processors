package org.clulab.dynet
import edu.cmu.dynet.{Dim, Expression, ExpressionVector, FloatVector, LookupParameter, ParameterCollection}
import org.clulab.embeddings.CompactWordEmbeddingMap
import org.slf4j.{Logger, LoggerFactory}
import ConstEmbeddingsGlove._
import org.clulab.utils.ConfigWithDefaults

/**
 * Implements the ConstEmbeddings using GloVe vectors
 * Note: two difference from the vanilla GloVe vectors are:
 * - Words with low frequency may be filtered out of the matrix
 * - The empty string ("") stands for UNK; it is typically computed by averaging the embeddings of the culled words
 */
class ConstEmbeddingsGlove(matrixResourceName: String, isResource:Boolean = true) extends ConstEmbeddings {

  /** These parameters are never updated, so we can share this object between models */
    /*
  private val parameters = new ParameterCollection()
  val (lookupParameters, w2i, wordVectors) = mkLookupParams()
  // All other values should be >= 0.
  val w2iUNK = w2i.getOrElse(UNK, -1)
  val w2iContainsUNK = w2iUNK != -1

  def mkLookupParams(): (LookupParameter, Map[String, Int], CompactWordEmbeddingMap) = {
    val wordVectors = CompactWordEmbeddingMap(matrixResourceName, resource = isResource, cached = false)
    val w2i = wordVectors.keys.toList.sorted.zipWithIndex.toMap

    val dim = wordVectors.columns
    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(dim))

    for(word <- wordVectors.keys) {
      wordLookupParameters.initialize(w2i(word), new FloatVector(wordVectors.get(word).get))
    }
    logger.debug(s"Completed loading word embeddings of dimension $dim for ${w2i.size} words.")

    (wordLookupParameters, w2i, wordVectors)
  }

  override def dim: Int = lookupParameters.dim().get(0).toInt
  */

  val wordVectors = CompactWordEmbeddingMap(matrixResourceName, resource = isResource, cached = false)

  override def dim: Int = wordVectors.get(UNK).get.length // TODO: we need the length method implemented in CompactWordEmbeddingMap

  override def mkEmbedding(word:String): Expression = {
    val vector = wordVectors.get(word).getOrElse(wordVectors.get(UNK).get) // TODO: we should have a getOrElse in the CompactWordEmbeddingMap
    Expression.input(Dim(vector.length), new FloatVector(vector))

    /*
    val w2iWord = w2i.getOrElse(word, {
      assert(w2iContainsUNK)
      w2iUNK
    })

    Expression.constLookup(lookupParameters, w2iWord)
    */
  }

}

object ConstEmbeddingsGlove {
  val logger:Logger = LoggerFactory.getLogger(classOf[ConstEmbeddingsGlove])

  private def UNK:String = "" // empty string for UNK

  def apply(configName:String = "org/clulab/glove.conf"): ConstEmbeddingsGlove = {
    val config = ConfigWithDefaults(configName)
    val matrixResourceName = config.getArgString("glove.matrixResourceName", None)
    val isResource = config.getArgBoolean("glove.isResource", Some(true))
    apply(matrixResourceName, isResource)
  }

  // This is not marked private for debugging purposes.
  var SINGLETON: Option[ConstEmbeddingsGlove] = None

  def apply(matrixResourceName: String, isResource: Boolean): ConstEmbeddingsGlove = {
    // These objects are read-only and they use a lot of RAM, so let's reuse them if they exist.
    // No ComputationGraph is touched during this process, so synchronization is not required.
    if (SINGLETON.isEmpty)
      SINGLETON = Some(new ConstEmbeddingsGlove(matrixResourceName, isResource))
    SINGLETON.get
  }
}
