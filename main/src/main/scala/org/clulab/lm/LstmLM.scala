package org.clulab.lm
import java.io.PrintWriter

import edu.cmu.dynet.{Dim, Expression, GruBuilder, LookupParameter, LstmBuilder, Parameter, ParameterCollection, RnnBuilder}
import org.clulab.sequences.LstmUtils
import LstmLM._
import org.clulab.lm.LstmLMTrainer.DROPOUT_PROB
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer

/**
 * Word embeddings from a BiLSTM concatenated with word-based character embeddings
 */
class LstmLM (val w2i: Map[String, Int],
              val t2i: Map[String, Int],
              val c2i: Map[Char, Int],
              val rnnDim: Int,
              val charRnnDim: Int,
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

    words.map(mkEmbedding2)

    /*
    val (wordIds, charIds) = sentenceToWords(words)
    val fwEmbeddings = wordIds.zip(charIds).map(mkEmbedding)
    val bwEmbeddings = fwEmbeddings.reverse

    val fwStates = LstmUtils.transduce(fwEmbeddings, wordFwRnnBuilder).toArray
    val bwStates = LstmUtils.transduce(bwEmbeddings, wordBwRnnBuilder).toArray.reverse
    assert(fwStates.length == bwStates.length)

    val states = new ArrayBuffer[Expression]()
    for(i <- fwStates.indices) {
      states += Expression.concatenate(fwStates(i), bwStates(i))
    }

    states

     */
  }

  def mkEmbedding2(word: String):Expression = {
    LstmUtils.mkWordEmbedding(word,
      w2i, wordLookupParameters,
      c2i, charLookupParameters,
      charFwRnnBuilder, charBwRnnBuilder)
  }

  def mkEmbedding(wordAndCharIds: Tuple2[Int, Array[Int]]): Expression = {
    val wid = wordAndCharIds._1
    val cids = wordAndCharIds._2

    val wordEmbedding = Expression.lookup(wordLookupParameters, wid)
    val charEmbedding = mkCharacterEmbedding(cids)

    Expression.concatenate(wordEmbedding, charEmbedding)
  }

  private def mkCharacterEmbedding(cids: Array[Int]): Expression = {
    val charEmbeddings = new ArrayBuffer[Expression]()
    for(i <- cids.indices) {
      charEmbeddings += Expression.lookup(charLookupParameters, cids(i))
    }
    val fwOut = LstmUtils.transduce(charEmbeddings, charFwRnnBuilder).last
    val bwOut = LstmUtils.transduce(charEmbeddings.reverse, charBwRnnBuilder).last
    Expression.concatenate(fwOut, bwOut)
  }

  def sentenceToWords(tokens: Iterable[String]): (Array[Int], Array[Array[Int]]) = {
    val wordIds = new ArrayBuffer[Int]()
    val chars = new ArrayBuffer[Array[Int]]()
    assert(w2i(LstmUtils.UNK_WORD) == 0)

    for(token <- tokens) {
      wordIds += w2i.getOrElse(token, 0) // w2i(LstmUtils.UNK_WORD)) // TODO: check!!

      val charIds = new ArrayBuffer[Int]()
      for(i <- token.indices) {
        val cid = c2i.getOrElse(token.charAt(i), 0) // id 0 is reserved for unknown chars
        charIds += cid
      }
      chars += charIds.toArray
    }

    (wordIds.toArray, chars.toArray)
  }

  override def dimensions: Int = (2 * charRnnDim + wordLookupParameters.dim().get(0).toInt) // 2 * rnnDim

  override def saveX2i(printWriter: PrintWriter): Unit = {
    LstmUtils.saveCharMap(printWriter, c2i, "c2i")
    LstmUtils.save(printWriter, w2i, "w2i")
    LstmUtils.save(printWriter, t2i, "t2i")
    LstmUtils.save(printWriter, wordLookupParameters.dim().get(0), "wordDim")
    LstmUtils.save(printWriter, charLookupParameters.dim().get(0), "charDim")
    LstmUtils.save(printWriter, charRnnDim, "charRnnDim")
    LstmUtils.save(printWriter, rnnDim, "rnnDim")
  }
}

object LstmLM {
  val logger:Logger = LoggerFactory.getLogger(classOf[LstmLM])

  /** Loads the LM inside a task specific model, *before* training the task */
  def load(modelBaseFilename:String, parameters: ParameterCollection): LstmLM = {
    logger.debug(s"Loading LM model from $modelBaseFilename...")
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
    val wordDim = byLineIntBuilder.build(x2iIterator)
    val charDim = byLineIntBuilder.build(x2iIterator)
    val charRnnDim = byLineIntBuilder.build(x2iIterator)
    val rnnDim = byLineIntBuilder.build(x2iIterator)

    logger.debug(s"Loaded a character map with ${c2i.keySet.size} entries.")
    logger.debug(s"Loaded a word map with ${w2i.keySet.size} entries and dimension $wordDim.")
    logger.debug(s"Loaded a word label map with ${t2i.keySet.size} entries.")

    //
    // make the loadable parameters
    //
    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(wordDim))
    val wordFwBuilder = new LstmBuilder(1, wordDim + 2 * charRnnDim, rnnDim, parameters)
    val wordBwBuilder = new LstmBuilder(1, wordDim + 2 * charRnnDim, rnnDim, parameters)

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(charDim))
    val charFwBuilder = new LstmBuilder(1, charDim, charRnnDim, parameters)
    val charBwBuilder = new LstmBuilder(1, charDim, charRnnDim, parameters)

    val fwO = parameters.addParameters(Dim(t2i.size, rnnDim))
    val bwO = parameters.addParameters(Dim(t2i.size, rnnDim))

    //
    // load these parameters from the DyNet model file
    //
    if(dynetFilename.nonEmpty) {
      // load the parameters above
      logger.debug(s"Loading pretrained LM model from $dynetFilename...")
      LstmUtils.loadParameters(dynetFilename.get, parameters, key = "/lstm")
    }

    charFwBuilder.disableDropout()
    charBwBuilder.disableDropout()
    wordFwBuilder.disableDropout()
    wordBwBuilder.disableDropout()

    val model = new LstmLM(
      w2i, t2i, c2i, rnnDim, charRnnDim, parameters,
      wordLookupParameters, wordFwBuilder, wordBwBuilder,
      charLookupParameters, charFwBuilder, charBwBuilder,
      fwO, bwO
    )

    model
  }
}
