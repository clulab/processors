package org.clulab.dynet

//import edu.cmu.dynet.{Dim, Expression, ExpressionVector, FloatVector, LookupParameter, ParameterCollection}
import org.clulab.scaladynet.expressions.Expression
import org.clulab.scaladynet.parameters.LookupParameter
import org.clulab.scaladynet.parameters.ParameterCollection
import org.clulab.scaladynet.utils.Dim
import org.clulab.scaladynet.vectors.ExpressionVector
import org.clulab.scaladynet.vectors.FloatVector

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
  private val parameters = new ParameterCollection()
  val (lookupParameters, w2i) = mkLookupParams()

  def mkLookupParams(): (LookupParameter, Map[String, Int]) = {
    val wordVectors = CompactWordEmbeddingMap(matrixResourceName, resource = isResource, cached = false)
    val w2i = wordVectors.keys.toList.sorted.zipWithIndex.toMap

    val dim = wordVectors.columns
    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(dim))

    for(word <- wordVectors.keys) {
      wordLookupParameters.initialize(w2i(word), new FloatVector(wordVectors.get(word).get))
    }
    logger.debug(s"Completed loading word embeddings of dimension $dim for ${w2i.size} words.")

    (wordLookupParameters, w2i)
  }

  override def dim: Int = lookupParameters.dim().get(0).toInt

  def get(word:String): Expression = {
    if(w2i.contains(word)) {
      Expression.constLookup(lookupParameters, w2i(word))
    } else {
      assert(w2i.contains(UNK))
      Expression.constLookup(lookupParameters, w2i(UNK))
    }
  }

  override def mkEmbeddings(words: Iterable[String]): ExpressionVector = {
    val ev = new ExpressionVector()

    for(word <- words) {
      ev.add(get(word))
    }

    ev
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

  private var SINGLETON: Option[ConstEmbeddingsGlove] = None

  def apply(matrixResourceName: String, isResource: Boolean): ConstEmbeddingsGlove = {
    // These objects are read-only and they use a lot of RAM, so let's reuse them if they exist.
    // No ComputationGraph is touched during this process, so synchronization is not required.
    if (SINGLETON.isEmpty)
      SINGLETON = Some(new ConstEmbeddingsGlove(matrixResourceName, isResource))
    SINGLETON.get
  }
}
