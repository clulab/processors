package org.clulab.lm

import java.io.PrintWriter

import edu.cmu.dynet.{Dim, Expression, LookupParameter, LstmBuilder, ParameterCollection, RnnBuilder}
import org.clulab.sequences.LstmUtils
import org.clulab.sequences.LstmUtils.{mkDynetFilename, mkX2iFilename}
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer

/**
 * Implements a RnnLM inspired by Lample et al. (2016) and ULMFit
 * @author Mihai
 */
class RnnLM(val w2i:Map[String, Int],
            val c2i:Map[Char, Int],
            val wordRnnStateSize: Int,
            val charRnnStateSize: Int,
            val parameters:ParameterCollection,
            val wordLookupParameters:LookupParameter,
            val charLookupParameters:LookupParameter,
            val charFwRnnBuilder:RnnBuilder,
            val charBwRnnBuilder:RnnBuilder,
            val wordFwRnnBuilder:RnnBuilder,
            val wordBwRnnBuilder:RnnBuilder) extends LM {

  /** Creates an overall word embedding by concatenating word and character embeddings */
  def mkEmbedding(word: String):Expression = {
    LstmUtils.mkWordEmbedding(word,
      w2i, wordLookupParameters,
      c2i, charLookupParameters,
      charFwRnnBuilder, charBwRnnBuilder)
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    val wordEmbedDim = wordLookupParameters.dim().get(0)
    val charEmbedDim = charLookupParameters.dim().get(0)

    LstmUtils.saveCharMap(printWriter, c2i, "c2i")
    LstmUtils.save(printWriter, w2i, "w2i")
    LstmUtils.save(printWriter, wordEmbedDim, "wordEmbedDim")
    LstmUtils.save(printWriter, charEmbedDim, "charEmbedDim")
    LstmUtils.save(printWriter, wordRnnStateSize, "wordRnnStateSize")
    LstmUtils.save(printWriter, charRnnStateSize, "charRnnStateSize")
  }

  override def mkEmbeddings(words: Iterable[String], doDropout:Boolean): Iterable[Expression] = {
    if(doDropout) {
      charFwRnnBuilder.setDropout(RnnLM.DROPOUT_PROB)
      charBwRnnBuilder.setDropout(RnnLM.DROPOUT_PROB)
      wordFwRnnBuilder.setDropout(RnnLM.DROPOUT_PROB)
      wordBwRnnBuilder.setDropout(RnnLM.DROPOUT_PROB)
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

  override def dimensions: Int = wordRnnStateSize * 2
}

object RnnLM {
  val logger:Logger = LoggerFactory.getLogger(classOf[RnnLM])

  val DROPOUT_PROB = 0.2f

  /** Loads the LM inside a task specific model, *before* training the task */
  def load(modelBaseFilename:String, parameters: ParameterCollection): RnnLM = {
    logger.debug(s"Loading RnnLM model from $modelBaseFilename...")
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
   * Loads the RnnLM model
   * @param x2iIterator iterates over the .x2i file
   * @param parameters ParameterCollection that holds all these parameters
   * @param dynetFilename If specified, load a pretrained model from here
   * @return the RnnLM object
   */
  def load(x2iIterator:Iterator[String],
           parameters: ParameterCollection,
           dynetFilename:Option[String] = None): RnnLM = {
    //
    // load the x2i info
    //
    val byLineCharMapBuilder = new LstmUtils.ByLineCharIntMapBuilder()
    val byLineStringMapBuilder = new LstmUtils.ByLineStringMapBuilder()
    val c2i = byLineCharMapBuilder.build(x2iIterator)
    val w2i = byLineStringMapBuilder.build(x2iIterator)
    val wordEmbedDim = new LstmUtils.ByLineIntBuilder().build(x2iIterator)
    val charEmbedDim = new LstmUtils.ByLineIntBuilder().build(x2iIterator)
    val wordRnnStateSize = new LstmUtils.ByLineIntBuilder().build(x2iIterator)
    val charRnnStateSize = new LstmUtils.ByLineIntBuilder().build(x2iIterator)

    logger.debug(s"\tLoaded a character map with ${c2i.keySet.size} entries.")
    logger.debug(s"\tLoaded a word map with ${w2i.keySet.size} entries.")
    logger.debug(s"\tUsing word embeddings of size $wordEmbedDim.")
    logger.debug(s"\tUsing char embeddings of size $charEmbedDim.")
    logger.debug(s"\tUsing two char LSTMs with state size $charRnnStateSize.")
    logger.debug(s"\tUsing two word LSTMs with state size $wordRnnStateSize.")

    //
    // make the loadable parameters
    //
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(wordEmbedDim))

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(charEmbedDim))
    val charFwRnnBuilder = new LstmBuilder(1, charEmbedDim, charRnnStateSize, parameters)
    val charBwRnnBuilder = new LstmBuilder(1, charEmbedDim, charRnnStateSize, parameters)

    val embeddingSize = 2 * charRnnStateSize + wordEmbedDim
    val fwBuilder = new LstmBuilder(1, embeddingSize, wordRnnStateSize, parameters)
    val bwBuilder = new LstmBuilder(1, embeddingSize, wordRnnStateSize, parameters)

    //
    // load these parameters from the DyNet model file
    //
    if(dynetFilename.nonEmpty) {
      // load the parameters above
      logger.debug(s"Loading pretrained RnnLM model from $dynetFilename...")
      LstmUtils.loadParameters(dynetFilename.get, parameters, key = "/rnnlm")
    }

    //
    // make the rest of the parameters
    // these parameters are randomly initialized, not pretrained
    //
    val model = new RnnLM(
      w2i, c2i, wordRnnStateSize, charRnnStateSize, parameters,
      lookupParameters, charLookupParameters,
      charFwRnnBuilder, charBwRnnBuilder,
      fwBuilder, bwBuilder
    )

    model
  }
}
