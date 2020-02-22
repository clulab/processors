package org.clulab.lm

import edu.cmu.dynet.{Dim, Expression, LookupParameter, LstmBuilder, ParameterCollection, RnnBuilder}
import org.clulab.sequences.LstmUtils
import org.clulab.sequences.LstmUtils.{mkDynetFilename, mkX2iFilename}
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

/** Implements the language model of Lample et al. (2016) */
class LampleLM(
  val w2i:Map[String, Int],
  val c2i:Map[Char, Int],
  val parameters:ParameterCollection,
  val lookupParameters:LookupParameter,
  val charLookupParameters:LookupParameter,
  val charFwRnnBuilder:RnnBuilder,
  val charBwRnnBuilder:RnnBuilder) extends LM {

  /** Creates an overall word embedding by concatenating word and character embeddings */
  def mkEmbedding(word: String):Expression =
    LstmUtils.mkWordEmbedding(word,
      w2i, lookupParameters,
      c2i, charLookupParameters,
      charFwRnnBuilder, charBwRnnBuilder)

  override def mkEmbeddings(words: Iterable[String]): Iterable[Expression] =
    words.map(mkEmbedding)
}

object LampleLM {
  val logger:Logger = LoggerFactory.getLogger(classOf[LampleLM])

  val CHAR_RNN_LAYERS = 1
  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16

  /** Loads the LM inside a task specific model, *before* training the task */
  def load(modelBaseFilename:String, parameters: ParameterCollection): LampleLM = {
    logger.debug(s"Loading Lample LM model from $modelBaseFilename...")
    val dynetFilename = mkDynetFilename(modelBaseFilename)
    val x2iFilename = mkX2iFilename(modelBaseFilename)

    //
    // load the x2i info
    //
    val (c2i, w2i, embeddingDim) = Serializer.using(LstmUtils.newSource(x2iFilename)) { source =>
      val byLineCharMapBuilder = new LstmUtils.ByLineCharIntMapBuilder()
      val byLineStringMapBuilder = new LstmUtils.ByLineStringMapBuilder()
      val lines = source.getLines()
      val c2i = byLineCharMapBuilder.build(lines)
      val w2i = byLineStringMapBuilder.build(lines)
      val dim = new LstmUtils.ByLineIntBuilder().build(lines)
      (c2i, w2i, dim)
    }
    logger.debug(s"Loaded a character map with ${c2i.keySet.size} entries.")
    logger.debug(s"Loaded a word map with ${w2i.keySet.size} entries.")

    //
    // mkParams
    //
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(embeddingDim))
    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwRnnBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwRnnBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val model = new LampleLM(
      w2i, c2i, parameters,
      lookupParameters, charLookupParameters,
      charFwRnnBuilder, charBwRnnBuilder
    )

    //
    // loadParameters
    //
    LstmUtils.loadParameters(dynetFilename, model.parameters, key = "/lample")

    model
  }
}
