package org.clulab.dynet
import com.typesafe.config.Config
import edu.cmu.dynet.{Dim, Expression, FloatVector, LookupParameter, ParameterCollection}
import org.clulab.dynet.ConstEmbeddingsGlove.mkLookupParams
import org.clulab.embeddings.{CompactWordEmbeddingMap, WordEmbeddingMap, WordEmbeddingMapPool}
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.utils.ConfigWithDefaults
import org.clulab.utils.StringUtils

/**
 * Implements the ConstEmbeddings as a thin wrapper around WordEmbeddingMap
 *   with additional functionality to produce embeddings as DyNet Expressions
 */
class ConstEmbeddingsGlove(val parameters: ParameterCollection,
                           val lookupParameters: LookupParameter,
                           val w2i: Map[String, Int],
                           val columns: Int) extends ConstEmbeddings {
  override def dim: Int = columns

  override def mkEmbedding(word: String): Expression = {
    val idx = w2i.getOrElse(word, 0) // 0 is unknown
    Expression.constLookup(lookupParameters, idx)
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
        // This is really meant to be a resource location, but we'll take a file if it's there.
        // val isResource = config.getArgBoolean("glove.isResource", Some(true))
        val name = StringUtils.afterLast(matrixResourceName, '/', all = true, keep = false)
        val location = StringUtils.beforeLast(matrixResourceName, '/', all = false, keep = true)

        val wordEmbeddingMap = {
          // First check to see if one is already available.
          val wordEmbeddingMapOpt = WordEmbeddingMapPool.get(name, compact = true)

          wordEmbeddingMapOpt.getOrElse {
            // If it should be created and cached, do this:
            //WordEmbeddingMapPool.getOrElseCreate(name, compact = true, location, location)

            // If it should just be created and should be hard-coded to be compact, do this:
            //CompactWordEmbeddingMap(matrixResourceName, resource = isResource, cached = false)

            // Load without storing it in the pool.
            WordEmbeddingMapPool.loadEmbedding(name, location, location, compact = true)
          }
        }

        val (parameters, lookupParameters, w2i, dim) = mkLookupParams(wordEmbeddingMap)

        SINGLETON = Some(new ConstEmbeddingsGlove(parameters, lookupParameters, w2i, dim))
      }
    }

    SINGLETON.get
  }

  protected def shift(range: Range, n: Int): Range = {
    require(!range.isInclusive)
    Range(range.start + n, range.end + n, range.step)
  }

  private def mkLookupParams(wordEmbeddingMap: WordEmbeddingMap): (ParameterCollection, LookupParameter, Map[String, Int], Int) = {
    val parameters = new ParameterCollection()
    val dim = wordEmbeddingMap.dim

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

    (parameters, wordLookupParameters, w2i, dim)
  }

  /** Make a standalone ConstEmbeddingsGlove not stored in the singleton */
  def apply(wordEmbeddingMap: WordEmbeddingMap): ConstEmbeddingsGlove = {
    val (parameters, lookupParameters, w2i, dim) = mkLookupParams(wordEmbeddingMap)
    new ConstEmbeddingsGlove(parameters, lookupParameters, w2i, dim)
  }
}
