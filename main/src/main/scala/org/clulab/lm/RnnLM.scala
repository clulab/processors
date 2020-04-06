package org.clulab.lm

import java.io.PrintWriter

import edu.cmu.dynet.{ComputationGraph, Dim, Expression, ExpressionVector, LookupParameter, LstmBuilder, Parameter, ParameterCollection, RMSPropTrainer, RnnBuilder}
import org.clulab.sequences.{LstmUtils, SafeTrainer}
import org.clulab.sequences.LstmUtils.{mkDynetFilename, mkX2iFilename}
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer
import org.clulab.struct.Counter

import scala.io.Source
import RnnLM._
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser


/**
 * Implements a RnnLM inspired by Lample et al. (2016) and ULMFit
 * @author Mihai
 */
class RnnLM(val w2i:Map[String, Int],
            val c2i:Map[Char, Int],
            val wordRnnStateSize: Int,
            val charRnnStateSize: Int,
            val lmLabelCount: Int,
            val parameters:ParameterCollection,
            val wordLookupParameters:LookupParameter,
            val charLookupParameters:LookupParameter,
            val charFwRnnBuilder:RnnBuilder,
            val charBwRnnBuilder:RnnBuilder,
            val wordFwRnnBuilder:RnnBuilder,
            val wordBwRnnBuilder:RnnBuilder,
            val fwO:Parameter,
            val bwO:Parameter) extends LM {

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
    LstmUtils.save(printWriter, lmLabelCount, "lmLabelCount")
  }

  def save(baseModelName: String): Unit = {
    val outDynetFilename = mkDynetFilename(baseModelName)
    val outX2iFilename = mkX2iFilename(baseModelName)

    new CloseableModelSaver(outDynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(parameters, "/rnnlm")
    }

    Serializer.using(LstmUtils.newPrintWriter(outX2iFilename)) { printWriter =>
      saveX2i(printWriter)
    }
  }

  private def setCharRnnDropout(doDropout: Boolean): Unit = {
    setRnnDropout(charFwRnnBuilder, doDropout)
    setRnnDropout(charBwRnnBuilder, doDropout)
  }

  private def setRnnDropout(rnnBuilder: RnnBuilder, doDropout: Boolean): Unit = {
    if(doDropout) {
      rnnBuilder.setDropout(RnnLM.DROPOUT_PROB)
    } else {
      rnnBuilder.disableDropout()
    }
  }

  override def mkEmbeddings(words: Iterable[String], doDropout:Boolean): Iterable[Expression] = {
    setCharRnnDropout(doDropout)
    setRnnDropout(wordFwRnnBuilder, doDropout)
    setRnnDropout(wordBwRnnBuilder, doDropout)

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

  /**
   * Pretrain this LM for next word prediction, in both directions
   * @param trainFile File with one sentence per line; each sentence must be pre-tokenized
   * @param devFileOpt Optional development file to report perplexity once in a while
   * @param lmLabelCount How many prediction labels to use
   * @param logCheckpoint When to printout the cummulative loss
   * @param saveCheckpoint When to save a model
   * @param batchSize Batch size in sentences; each actual sentences counts as 2 due to the bi traversal
   */
  def trainLM(trainFile: String,
              devFileOpt: Option[String],
              lmLabelCount: Int,
              logCheckpoint: Int,
              saveCheckpoint: Int,
              batchSize: Int): Unit = {
    val t2i = mkT2i(trainFile, lmLabelCount)
    val trainer = SafeTrainer(new RMSPropTrainer(parameters))

    // train the fw and bw word LSTMs on all sentences in training
    val source = Source.fromFile(trainFile)
    var sentCount = 0
    var cummulativeLoss = 0.0
    var numTagged = 0

    // start the first batch
    ComputationGraph.renew()
    var batchLosses = new ExpressionVector()

    for (sentence <- source.getLines()) {
      val words = sentence.split("\\s+") // these sentence must be pre-tokenized!

      //
      // left-to-right prediction
      //
      val fwIn = words
      val fwEmissionScores = emissionScoresAsExpressions(fwIn, wordFwRnnBuilder, fwO, doDropout = true)
      val fwLoss = languageModelLoss(fwEmissionScores, fwIn, t2i)
      batchLosses.add(fwLoss)

      //
      // right-to-left prediction
      //
      val bwIn = words.reverse
      val bwEmissionScores = emissionScoresAsExpressions(bwIn, wordBwRnnBuilder, bwO, doDropout = true)
      val bwLoss = languageModelLoss(bwEmissionScores, bwIn, t2i)
      batchLosses.add(bwLoss)

      //
      // book keeping
      //
      sentCount += 1
      numTagged += words.length

      //
      // backprop
      // we do this only when the batch is full
      //
      if (batchLosses.size >= batchSize) {
        val comboLoss = Expression.sum(batchLosses) / batchLosses.size
        cummulativeLoss += comboLoss.value().toFloat()
        ComputationGraph.backward(comboLoss)
        trainer.update(parameters)

        // report perplexity if a dev file is available
        if (sentCount % saveCheckpoint == 0 && devFileOpt.nonEmpty) {
          reportPerplexity(devFileOpt.get, t2i)
        }

        // reset for the next batch
        ComputationGraph.renew()
        batchLosses = new ArrayBuffer[Expression]()
        //println("Renewed graph!")
      }

      //
      // reporting and model saving
      //
      if (sentCount % logCheckpoint == 0) {
        logger.debug(s"Processed $sentCount sentences. Cummulative loss: ${cummulativeLoss / numTagged}.")

        // save a model when we hit a save checkpoint
        if (sentCount % saveCheckpoint == 0) {
          val baseModelName = s"rnnlm_s$sentCount"
          save(baseModelName)
        }
      }
    }

    source.close()
  }

  private def emissionScoresAsExpressions(words: Array[String],
                                          rnnBuilder: RnnBuilder,
                                          pO: Parameter,
                                          doDropout: Boolean): ExpressionVector = {
    setCharRnnDropout(doDropout)
    setRnnDropout(rnnBuilder, doDropout)

    val embeddings = words.map(mkEmbedding)
    val states = LstmUtils.transduce(embeddings, rnnBuilder)

    val O = Expression.parameter(pO)
    val emissionScores = new ExpressionVector()
    for(s <- states) {
      emissionScores.add(O * s)
    }

    emissionScores
  }

  private def languageModelLoss(emissionScoresForSeq: ExpressionVector,
                                labels: Array[String],
                                t2i: Map[String, Int]): Expression = {

    val goldLosses = new ExpressionVector()

    for (i <- emissionScoresForSeq.indices) {
      val goldTid = getGoldTagId(labels, i, t2i)

      // emissionScoresForSeq(i) = all tag emission scores for the word at position i
      goldLosses.add(Expression.pickNegLogSoftmax(emissionScoresForSeq(i), goldTid))
    }

    Expression.sum(goldLosses)
  }

  private def getGoldTagId(labels: Array[String], i:Int, t2i: Map[String, Int]): Int = {
    if(i < labels.length - 1) {
      val nextWord = labels(i + 1)
      t2i.getOrElse(nextWord, 0) // 0 is reserved for the UNK word
    } else {
      1 // 1 is reserved for the EOS word
    }
  }

  private def mkT2i(trainFile: String, lmLabelCount:Int): Map[String, Int] = {
    logger.debug(s"Counting words in file $trainFile...")
    val counts = new Counter[String]()
    val source = Source.fromFile(trainFile)
    var sentCount = 0
    for(line <- source.getLines()) {
      val tokens = line.split("\\s+")
      for(word <- tokens) {
        counts.incrementCount(word)
      }
      sentCount += 1
    }
    source.close()
    logger.debug("Counting completed.")
    logger.debug(s"Found ${counts.size} unique words.")

    val sortedWords = counts.sorted(descending = true)
    val labels = new ArrayBuffer[String]()
    labels += LstmUtils.UNK_WORD
    labels += LstmUtils.EOS_WORD

    var done = false
    var count = 2 // we already added UNK and EOS
    for(sortedWord <- sortedWords if ! done) {
      labels += sortedWord._1
      count += 1
      if(count >= lmLabelCount) {
        done = true
      }
    }

    logger.debug(s"The LM label set contains ${labels.size} labels.")
    labels.zipWithIndex.toMap
  }

  def reportPerplexity(devFileName: String, t2i: Map[String, Int]): Unit = {
    val source = Source.fromFile(devFileName)
    var sentCount = 0
    var cummulativeFwPerplexity = 0.0
    var cummulativeBwPerplexity = 0.0

    logger.debug("Computing perplexity in dev...")
    for(sentence <- source.getLines()) {
      val words = sentence.split("\\s+")
      ComputationGraph.renew()

      val fwIn = words
      val fwEmissionScores = emissionScoresAsExpressions(fwIn, wordFwRnnBuilder, fwO, doDropout = false) // no dropout during testing!
      val fwPp = perplexity(fwEmissionScores, fwIn, t2i)

      val bwIn = words.reverse
      val bwEmissionScores = emissionScoresAsExpressions(bwIn, wordBwRnnBuilder, bwO, doDropout = false)
      val bwPp = perplexity(bwEmissionScores, bwIn, t2i)

      cummulativeFwPerplexity += fwPp
      cummulativeBwPerplexity += bwPp
      sentCount += 1
    }
    source.close()
    logger.info(s"Average forward perplexity: ${cummulativeFwPerplexity / sentCount.toDouble}")
    logger.info(s"Average backward perplexity: ${cummulativeBwPerplexity / sentCount.toDouble}")
  }

  /** Computes perplexity for this sentence */
  def perplexity(emissionScoresForSeq: ExpressionVector, words: Array[String], t2i: Map[String, Int]): Double = {
    var pp = 1.0
    for(i <- emissionScoresForSeq.indices) {
      val goldTid = getGoldTagId(words, i, t2i)
      val prob = Expression.pick(Expression.softmax(emissionScoresForSeq(i)), goldTid)
      pp *= math.pow(1.0 / prob.value().toFloat(), 1.0 / words.length.toDouble)
    }
    pp
  }
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
    val lmLabelCount = new LstmUtils.ByLineIntBuilder().build(x2iIterator)

    logger.debug(s"\tLoaded a character map with ${c2i.keySet.size} entries.")
    logger.debug(s"\tLoaded a word map with ${w2i.keySet.size} entries.")
    logger.debug(s"\tUsing word embeddings of size $wordEmbedDim.")
    logger.debug(s"\tUsing char embeddings of size $charEmbedDim.")
    logger.debug(s"\tUsing two char LSTMs with state size $charRnnStateSize.")
    logger.debug(s"\tUsing two word LSTMs with state size $wordRnnStateSize.")
    logger.debug(s"\tWhen training the LM using $lmLabelCount labels.")

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

    val fwO = parameters.addParameters(Dim(lmLabelCount, wordRnnStateSize))
    val bwO = parameters.addParameters(Dim(lmLabelCount, wordRnnStateSize))

    //
    // load these parameters from the DyNet model file
    //
    if(dynetFilename.nonEmpty) {
      // load the parameters above
      logger.debug(s"Loading pretrained RnnLM model from $dynetFilename...")
      LstmUtils.loadParameters(dynetFilename.get, parameters, key = "/rnnlm")
    }

    val model = new RnnLM(
      w2i, c2i, wordRnnStateSize, charRnnStateSize, lmLabelCount, parameters,
      lookupParameters, charLookupParameters,
      charFwRnnBuilder, charBwRnnBuilder,
      fwBuilder, bwBuilder, fwO, bwO
    )

    model
  }
}


