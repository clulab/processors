package org.clulab.lm

import java.io.PrintWriter

import edu.cmu.dynet.{Dim, Expression, LookupParameter, LstmBuilder, ParameterCollection, RnnBuilder}
import org.clulab.sequences.LstmCrfMtlParameters.{RNN_LAYERS, RNN_STATE_SIZE}
import org.clulab.sequences.LstmUtils
import org.clulab.sequences.LstmUtils.{mkDynetFilename, mkX2iFilename}
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer

/** Implements the language model of Lample et al. (2016) */
class LampleLM2(
                val w2i:Map[String, Int],
                val c2i:Map[Char, Int],
                val parameters:ParameterCollection,
                val lookupParameters:LookupParameter,
                val charLookupParameters:LookupParameter,
                val charFwRnnBuilder:RnnBuilder,
                val charBwRnnBuilder:RnnBuilder,
                val wordFwRnnBuilder:RnnBuilder,
                val wordBwRnnBuilder:RnnBuilder) extends LM {

  /** Creates an overall word embedding by concatenating word and character embeddings */
  def mkEmbedding(word: String):Expression = {
    LstmUtils.mkWordEmbedding(word,
      w2i, lookupParameters,
      c2i, charLookupParameters,
      charFwRnnBuilder, charBwRnnBuilder)
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    val dim = lookupParameters.dim().get(0)

    LstmUtils.saveCharMap(printWriter, c2i, "c2i")
    LstmUtils.save(printWriter, w2i, "w2i")
    LstmUtils.save(printWriter, dim, "dim")
  }

  override def mkEmbeddings(words: Iterable[String], doDropout:Boolean): Iterable[Expression] = {
    if(doDropout) {
      charFwRnnBuilder.setDropout(LampleLM2.DROPOUT_PROB)
      charBwRnnBuilder.setDropout(LampleLM2.DROPOUT_PROB)
      wordFwRnnBuilder.setDropout(LampleLM2.DROPOUT_PROB)
      wordBwRnnBuilder.setDropout(LampleLM2.DROPOUT_PROB)
    } else {
      charFwRnnBuilder.disableDropout()
      charBwRnnBuilder.disableDropout()
      wordFwRnnBuilder.disableDropout()
      wordBwRnnBuilder.disableDropout()
    }

    val embeddings = words.map(mkEmbedding)

    val fwEmbeddings = embeddings.toArray
    val fwStates = LstmUtils.transduce(fwEmbeddings, wordFwRnnBuilder).toArray
    val bwEmbeddings = fwEmbeddings.reverse
    val bwStates = LstmUtils.transduce(bwEmbeddings, wordBwRnnBuilder).toArray.reverse
    assert(fwStates.length == bwStates.length)

    val states = new ArrayBuffer[Expression]()
    for(i <- fwStates.indices) {
      states += Expression.concatenate(fwStates(i), bwStates(i))
    }

    states
  }

  override def dimensions: Int =
    // (2 * LampleLM2.CHAR_RNN_STATE_SIZE) + lookupParameters.dim().get(0).toInt
    RNN_STATE_SIZE * 2
}

object LampleLM2 {
  val logger:Logger = LoggerFactory.getLogger(classOf[LampleLM2])

  val CHAR_RNN_LAYERS = 1
  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16
  val DROPOUT_PROB = 0.2f

  /** Loads the LM inside a task specific model, *before* training the task */
  def load(modelBaseFilename:String, parameters: ParameterCollection): LampleLM2 = {
    logger.debug(s"Loading Lample LM model from $modelBaseFilename...")
    val dynetFilename = mkDynetFilename(modelBaseFilename)
    val x2iFilename = mkX2iFilename(modelBaseFilename)

    //
    // load the x2i info, construct the parameters, and load them
    //
    val model = Serializer.using(LstmUtils.newSource(x2iFilename)) { source =>
      val lines = source.getLines()
      load(lines, parameters, Some(dynetFilename))
    }

    model
  }

  /**
   * Loads the Lample LM model
   * @param x2iIterator iterates over the .x2i file
   * @param parameters ParameterCollection that holds all these parameters
   * @param dynetFilename If specified, load a pretrained model from here
   * @return the LampleLM object
   */
  def load(x2iIterator:Iterator[String],
           parameters: ParameterCollection,
           dynetFilename:Option[String] = None): LampleLM2 = {
    //
    // load the x2i info
    //
    val byLineCharMapBuilder = new LstmUtils.ByLineCharIntMapBuilder()
    val byLineStringMapBuilder = new LstmUtils.ByLineStringMapBuilder()
    val c2i = byLineCharMapBuilder.build(x2iIterator)
    val w2i = byLineStringMapBuilder.build(x2iIterator)
    val embeddingDim = new LstmUtils.ByLineIntBuilder().build(x2iIterator)

    logger.debug(s"Loaded a character map with ${c2i.keySet.size} entries.")
    logger.debug(s"Loaded a word map with ${w2i.keySet.size} entries.")
    logger.debug(s"Using word embeddings of size $embeddingDim.")

    //
    // make the loadable parameters
    //
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(embeddingDim))

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwRnnBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwRnnBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)

    val embeddingSize = 2 * CHAR_RNN_STATE_SIZE + embeddingDim
    val fwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)
    val bwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)

    //
    // load these parameters from the DyNet model file
    //
    if(dynetFilename.nonEmpty) {
      // load the parameters above
      logger.debug(s"Loading pretrained Lample LM model from $dynetFilename...")
      LstmUtils.loadParameters(dynetFilename.get, parameters, key = "/lample")
    }

    //
    // make the rest of the parameters
    // these parameters are randomly initialized, not pretrained
    //
    val model = new LampleLM2(
      w2i, c2i, parameters,
      lookupParameters, charLookupParameters,
      charFwRnnBuilder, charBwRnnBuilder,
      fwBuilder, bwBuilder
    )

    model
  }
}
