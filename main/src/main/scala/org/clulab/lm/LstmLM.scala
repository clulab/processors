package org.clulab.lm
import java.io.PrintWriter

import edu.cmu.dynet.{Dim, Expression, GruBuilder, LookupParameter, LstmBuilder, Parameter, ParameterCollection, RnnBuilder}
import org.clulab.sequences.LstmUtils
import LstmLM._
import org.clulab.lm.LstmLMTrainer.{DROPOUT_PROB, WORD_EMBEDDING_SIZE, WORD_RNN_STATE_SIZE}
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer

/**
 * Word embeddings from a BiLSTM concatenated with word-based character embeddings
 */
class LstmLM (val w2i: Map[String, Int],
              val t2i: Map[String, Int],
              val c2i: Map[Char, Int],
              val parameters: ParameterCollection,
              val wordLookupParameters: LookupParameter,
              val wordFwRnnBuilder: RnnBuilder,
              val wordBwRnnBuilder: RnnBuilder,
              val charLookupParameters: LookupParameter,
              val charFwRnnBuilder: RnnBuilder,
              val charBwRnnBuilder: RnnBuilder,
              val fwO: Parameter,
              val bwO: Parameter) extends LM {

  override def mkEmbeddings(words: Iterable[String], doDropout:Boolean): Iterable[Expression] = {
    if(doDropout) {
      wordFwRnnBuilder.setDropout(DROPOUT_PROB)
      wordBwRnnBuilder.setDropout(DROPOUT_PROB)
      charFwRnnBuilder.setDropout(DROPOUT_PROB)
      charBwRnnBuilder.setDropout(DROPOUT_PROB)
    } else {
      wordFwRnnBuilder.disableDropout()
      wordBwRnnBuilder.disableDropout()
      charFwRnnBuilder.disableDropout()
      charBwRnnBuilder.disableDropout()
    }

    val wordIds = sentenceToWords(words)

    // TODO: add word LSTMs and char LSTMs here!
    null
  }

  def sentenceToWords(tokens: Iterable[String]): Array[Int] = {
    val wordIds = new ArrayBuffer[Int]()

    for(token <- tokens) {
      wordIds += w2i.getOrElse(token, w2i(LstmUtils.UNK_WORD))
    }

    wordIds.toArray
  }

  override def dimensions: Int = {
    2 * LstmLMTrainer.WORD_RNN_STATE_SIZE + 2 * CHAR_RNN_STATE_SIZE
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    LstmUtils.saveCharMap(printWriter, c2i, "c2i")
    LstmUtils.save(printWriter, w2i, "w2i")
    LstmUtils.save(printWriter, t2i, "t2i")
    LstmUtils.save(printWriter, wordLookupParameters.dim().get(0), "dim")
  }
}

object LstmLM {
  val logger:Logger = LoggerFactory.getLogger(classOf[LstmLM])

  val CHAR_RNN_LAYERS = 1
  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16

  /** Loads the LM inside a task specific model, *before* training the task */
  def load(modelBaseFilename:String, parameters: ParameterCollection): LstmLM = {
    logger.debug(s"Loading FLAIR LM model from $modelBaseFilename...")
    val dynetFilename = LstmUtils.mkDynetFilename(modelBaseFilename)
    val x2iFilename = LstmUtils.mkX2iFilename(modelBaseFilename)

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
   * Loads the LstmLM model
   * @param x2iIterator iterates over the .x2i file
   * @param parameters ParameterCollection that holds all these parameters
   * @param dynetFilename If specified, load a pretrained model from here
   * @return the LstmLM object
   */
  def load(x2iIterator:Iterator[String],
           parameters: ParameterCollection,
           dynetFilename:Option[String] = None): LstmLM = {
    //
    // load the x2i info
    //
    val byLineCharMapBuilder = new LstmUtils.ByLineCharIntMapBuilder()
    val byLineStringMapBuilder = new LstmUtils.ByLineStringMapBuilder()
    val byLineIntBuilder = new LstmUtils.ByLineIntBuilder()
    val c2i = byLineCharMapBuilder.build(x2iIterator)
    val w2i = byLineStringMapBuilder.build(x2iIterator)
    val t2i = byLineStringMapBuilder.build(x2iIterator)
    val wordEmbeddingDim = byLineIntBuilder.build(x2iIterator)

    logger.debug(s"Loaded a character map with ${c2i.keySet.size} entries.")
    logger.debug(s"Loaded a word map with ${w2i.keySet.size} entries and dimension $wordEmbeddingDim.")
    logger.debug(s"Loaded a word label map with ${t2i.keySet.size} entries.")

    //
    // make the loadable parameters
    //
    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwBuilder = new GruBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwBuilder = new GruBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(wordEmbeddingDim))
    val wordFwBuilder = new LstmBuilder(1, WORD_EMBEDDING_SIZE, WORD_RNN_STATE_SIZE, parameters)
    val wordBwBuilder = new LstmBuilder(1, WORD_EMBEDDING_SIZE, WORD_RNN_STATE_SIZE, parameters)
    val fwO = parameters.addParameters(Dim(c2i.size, WORD_RNN_STATE_SIZE))
    val bwO = parameters.addParameters(Dim(c2i.size, WORD_RNN_STATE_SIZE))

    //
    // load these parameters from the DyNet model file
    //
    if(dynetFilename.nonEmpty) {
      // load the parameters above
      logger.debug(s"Loading pretrained LstmLM model from $dynetFilename...")
      LstmUtils.loadParameters(dynetFilename.get, parameters, key = "/lstm")
    }

    charFwBuilder.disableDropout()
    charBwBuilder.disableDropout()
    wordFwBuilder.disableDropout()
    wordBwBuilder.disableDropout()

    val model = new LstmLM(
      w2i, t2i, c2i, parameters,
      wordLookupParameters, wordFwBuilder, wordBwBuilder,
      charLookupParameters, charFwBuilder, charBwBuilder,
      fwO, bwO
    )

    model
  }
}
