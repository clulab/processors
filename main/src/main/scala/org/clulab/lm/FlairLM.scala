package org.clulab.lm

import java.io.PrintWriter

import edu.cmu.dynet.Expression.lookup
import edu.cmu.dynet.{Dim, Expression, ExpressionVector, GruBuilder, LookupParameter, Parameter, ParameterCollection, RnnBuilder}
import org.clulab.lm.FlairTrainer.{CHAR_EMBEDDING_SIZE, CHAR_RNN_LAYERS, CHAR_RNN_STATE_SIZE}
import org.clulab.sequences.LstmUtils
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer

class FlairLM ( val w2i: Map[String, Int],
                val c2i: Map[Char, Int],
                val parameters: ParameterCollection,
                val wordLookupParameters: LookupParameter,
                val charLookupParameters: LookupParameter,
                val charFwRnnBuilder: RnnBuilder,
                val charBwRnnBuilder: RnnBuilder,
                val fwO: Parameter,
                val bwO: Parameter) extends LM {

  override def mkEmbeddings(words: Iterable[String], posTags: Option[Iterable[String]], predPosition: Option[Int], doDropout:Boolean): Iterable[Expression] = {

    if(doDropout) {
      charFwRnnBuilder.setDropout(FlairTrainer.DROPOUT_PROB)
      charBwRnnBuilder.setDropout(FlairTrainer.DROPOUT_PROB)
    } else {
      charFwRnnBuilder.disableDropout()
      charBwRnnBuilder.disableDropout()
    }

    val (text, firstLastOffsets) = wordsToCharacters(words)
    assert(words.size == firstLastOffsets.length)
    //println(s"mkEmbeddings for sentence: ${text.mkString("")}")
    //println(s"First/last offsets: ${firstLastOffsets.mkString(" ")}")

    val fwIn = text.map(mkCharEmbedding)
    val fwOut = LstmUtils.transduce(fwIn, charFwRnnBuilder).toArray
    assert(fwOut.length == text.length)
    val bwIn = fwIn.reverse
    val bwOut = LstmUtils.transduce(bwIn, charBwRnnBuilder).toArray.reverse
    assert(bwOut.length == text.length)

    val states = new ExpressionVector()
    for(w <- firstLastOffsets) {
      val endExp = fwOut(w._2)
      val beginExp = bwOut(w._1)

      val wordEmb =
        if(w2i.contains(w._3)) {
          lookup(wordLookupParameters, w2i(w._3))
        } else {
          lookup(wordLookupParameters, 0)
        }

      val state = Expression.concatenate(wordEmb, endExp, beginExp)
      states.add(state)
    }

    assert(states.length == firstLastOffsets.length)
    states
  }

  def mkCharEmbedding(c:Char): Expression = {
    val charEmbedding = lookup(charLookupParameters, c2i(c))
    charEmbedding
  }

  def wordsToCharacters(words:Iterable[String]): (Array[Char], Array[(Int, Int, String)]) = {
    val charBuffer = new ArrayBuffer[Char]()
    val firstLastOffsets = new ArrayBuffer[(Int, Int, String)]()
    for(word <- words) {
      val first = charBuffer.size
      val last = first + word.length - 1
      firstLastOffsets += Tuple3(first, last, word)

      for(i <- word.indices) {
        val c = word.charAt(i)
        if (c2i.contains(c)) {
          charBuffer += c
        } else {
          charBuffer += FlairTrainer.UNKNOWN_CHAR
        }
      }
      charBuffer += ' '
    }
    (charBuffer.toArray, firstLastOffsets.toArray)
  }

  override def dimensions: Int = {
    (wordLookupParameters.dim().get(0) + 2 * FlairTrainer.CHAR_RNN_STATE_SIZE).toInt
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    LstmUtils.saveCharMap(printWriter, c2i, "c2i")
    LstmUtils.save(printWriter, charLookupParameters.dim().get(0), "charDim")
    LstmUtils.save(printWriter, w2i, "w2i")
    LstmUtils.save(printWriter, wordLookupParameters.dim().get(0), "wordDim")
  }
}

object FlairLM {
  val logger:Logger = LoggerFactory.getLogger(classOf[FlairLM])

  /** Loads the LM inside a task specific model, *before* training the task */
  def load(modelBaseFilename:String, parameters: ParameterCollection): FlairLM = {
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
   * Loads the FLAIR LM model
   * @param x2iIterator iterates over the .x2i file
   * @param parameters ParameterCollection that holds all these parameters
   * @param dynetFilename If specified, load a pretrained model from here
   * @return the FlairLM object
   */
  def load(x2iIterator:Iterator[String],
           parameters: ParameterCollection,
           dynetFilename:Option[String] = None): FlairLM = {
    //
    // load the x2i info
    //
    val byLineCharMapBuilder = new LstmUtils.ByLineCharIntMapBuilder()
    val byLineStringMapBuilder = new LstmUtils.ByLineStringMapBuilder()
    val byLineIntBuilder = new LstmUtils.ByLineIntBuilder()
    val c2i = byLineCharMapBuilder.build(x2iIterator)
    val charEmbeddingDim = byLineIntBuilder.build(x2iIterator)
    val w2i = byLineStringMapBuilder.build(x2iIterator)
    val wordEmbeddingDim = byLineIntBuilder.build(x2iIterator)

    logger.debug(s"Loaded a character map with ${c2i.keySet.size} entries and dimension $charEmbeddingDim.")
    logger.debug(s"Loaded a word map with ${w2i.keySet.size} entries and dimension $wordEmbeddingDim.")

    //
    // make the loadable parameters
    //
    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwBuilder = new GruBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwBuilder = new GruBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(wordEmbeddingDim))
    val fwO = parameters.addParameters(Dim(c2i.size, CHAR_RNN_STATE_SIZE))
    val bwO = parameters.addParameters(Dim(c2i.size, CHAR_RNN_STATE_SIZE))

    //
    // load these parameters from the DyNet model file
    //
    if(dynetFilename.nonEmpty) {
      // load the parameters above
      logger.debug(s"Loading pretrained FLAIR LM model from $dynetFilename...")
      LstmUtils.loadParameters(dynetFilename.get, parameters, key = "/flair")
    }

    charFwBuilder.disableDropout()
    charBwBuilder.disableDropout()

    val model = new FlairLM(
      w2i, c2i, parameters,
      lookupParameters, charLookupParameters,
      charFwBuilder, charBwBuilder,
      fwO, bwO
    )

    model
  }
}
