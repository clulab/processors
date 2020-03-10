package org.clulab.lm

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{ComputationGraph, Dim, Expression, ExpressionVector, LookupParameter, LstmBuilder, Parameter, ParameterCollection, RMSPropTrainer, RnnBuilder}
import org.clulab.sequences.{LstmUtils, SafeTrainer}
import org.clulab.sequences.LstmUtils.{initializeDyNet, mkDynetFilename, mkX2iFilename}
import org.clulab.struct.Counter
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.io.Source
import LstmLMTrainer._
import edu.cmu.dynet.Expression.{pick, pickNegLogSoftmax, softmax, sum}
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.utils.Serializer

import scala.collection.mutable.ArrayBuffer

import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser

class LstmLMTrainer (val w2i: Map[String, Int],
                     val t2i: Map[String, Int],
                     val parameters: ParameterCollection,
                     val wordLookupParameters: LookupParameter,
                     val wordFwRnnBuilder: RnnBuilder,
                     val wordBwRnnBuilder: RnnBuilder,
                     val fwO: Parameter,
                     val bwO: Parameter) {

  val eosTagId:Int = t2i(LstmUtils.EOS_WORD)

  def mkTrainer(): SafeTrainer = {
    SafeTrainer(new RMSPropTrainer(parameters))
  }

  /**
   * Trains the LM from the text in this file
   * The file must contain a sentence per line,
   *   with the white spaces between tokens normalized to a single space
   * @param trainFileName The name of the file with training sentences
   */
  def train( trainFileName:String,
             devFileName:Option[String],
             logCheckpoint:Int,
             saveCheckpoint:Int): Unit = {
    // initialize optimizer
    val trainer = mkTrainer()

    // train the fw and bw word LSTMs on all sentences in training
    val source = Source.fromFile(trainFileName)
    var sentCount = 0
    var cummulativeLoss = 0.0
    var numTagged = 0

    // start the first batch
    ComputationGraph.renew()
    var batchLosses = new ExpressionVector()

    for(sentence <- source.getLines()) {
      val (words, tags) = sentenceToWords(sentence)

      //
      // left-to-right prediction
      //
      val fwIn = words
      val fwTags = tags
      val fwEmissionScores = emissionScoresAsExpressions(fwIn, fwTags, wordFwRnnBuilder, fwO, doDropout = true)
      val fwLoss = languageModelLoss(fwEmissionScores, fwTags)
      batchLosses.add(fwLoss)

      //
      // right-to-left prediction
      //
      val bwIn = words.reverse
      val bwTags = tags.reverse
      val bwEmissionScores = emissionScoresAsExpressions(bwIn, bwTags, wordBwRnnBuilder, bwO, doDropout = true)
      val bwLoss = languageModelLoss(bwEmissionScores, bwTags)
      batchLosses.add(bwLoss)

      //
      // book keeping
      //
      sentCount += 1
      numTagged += words.length + 1

      //
      // backprop
      // we do this only when the batch is full
      //
      if(batchLosses.size >= BATCH_SIZE) {
        val comboLoss = sum(batchLosses) / batchLosses.size
        cummulativeLoss += comboLoss.value().toFloat()
        ComputationGraph.backward(comboLoss)
        trainer.update(parameters)

        // report perplexity if a dev file is available
        if(sentCount % saveCheckpoint == 0 && devFileName.nonEmpty){
          reportPerplexity(devFileName.get)
        }

        // reset for the next batch
        ComputationGraph.renew()
        batchLosses = new ArrayBuffer[Expression]()
        //println("Renewed graph!")
      }

      //
      // reporting and model saving
      //
      if(sentCount % logCheckpoint == 0) {
        logger.debug(s"Processed $sentCount sentences. Cummulative loss: ${cummulativeLoss / numTagged}.")

        // save a model when we hit a save checkpoint
        if(sentCount % saveCheckpoint == 0){
          val baseModelName = s"lstmlm_s$sentCount"
          save(baseModelName)
        }
      }
    }
  }

  def emissionScoresAsExpressions(words: Array[Int],
                                  tags: Array[Int],
                                  rnnBuilder: RnnBuilder,
                                  pO:Parameter,
                                  doDropout:Boolean = false): ExpressionVector = {
    val embeddings = words.map(mkEmbedding)

    if(doDropout) {
      rnnBuilder.setDropout(DROPOUT_PROB)
    } else {
      rnnBuilder.disableDropout()
    }

    val states = LstmUtils.transduce(embeddings, rnnBuilder)

    val O = Expression.parameter(pO)
    val emissionScores = new ExpressionVector()
    for(s <- states) {
      emissionScores.add(O * s)
    }

    emissionScores
  }

  def mkEmbedding(wi: Int): Expression = Expression.lookup(wordLookupParameters, wi)

  /** Greedy loss function, ignoring transition scores */
  def languageModelLoss(emissionScoresForSeq:ExpressionVector,
                        tags:Array[Int]): Expression = {

    val goldLosses = new ExpressionVector()

    for(i <- emissionScoresForSeq.indices) {
      val goldTid = getGoldTagId(tags, i)

      // emissionScoresForSeq(i) = all tag emission scores for the word at position i
      goldLosses.add(pickNegLogSoftmax(emissionScoresForSeq(i), goldTid))
    }

    sum(goldLosses)
  }

  def getGoldTagId(tags: Array[Int], i:Int): Int = {
    if (i < tags.length - 1) tags(i + 1)
    else eosTagId
  }

  def reportPerplexity(devFileName: String): Unit = {
    val source = Source.fromFile(devFileName)
    var sentCount = 0
    var cummulativeFwPerplexity = 0.0
    var cummulativeBwPerplexity = 0.0

    logger.debug("Computing perplexity in dev...")
    for(sentence <- source.getLines()) {
      val (words, tags) = sentenceToWords(sentence)
      ComputationGraph.renew()

      val fwIn = words
      val fwTags = tags
      val fwEmissionScores = emissionScoresAsExpressions(fwIn, fwTags, wordFwRnnBuilder, fwO) // no dropout during testing!
      val fwPp = perplexity(fwEmissionScores, fwTags)

      val bwIn = words.reverse
      val bwTags = tags.reverse
      val bwEmissionScores = emissionScoresAsExpressions(bwIn, bwTags, wordBwRnnBuilder, bwO)
      val bwPp = perplexity(bwEmissionScores, bwTags)

      cummulativeFwPerplexity += fwPp
      cummulativeBwPerplexity += bwPp
      sentCount += 1
    }
    source.close()
    logger.info(s"Average forward perplexity: ${cummulativeFwPerplexity / sentCount.toDouble}")
    logger.info(s"Average backward perplexity: ${cummulativeBwPerplexity / sentCount.toDouble}")
  }

  /** Computes perplexity for this sentence */
  def perplexity(emissionScoresForSeq: ExpressionVector, tags: Array[Int]): Double = {
    var pp = 1.0
    for(i <- emissionScoresForSeq.indices) {
      val goldTid = getGoldTagId(tags, i)
      val prob = pick(softmax(emissionScoresForSeq(i)), goldTid)
      pp *= math.pow(1.0 / prob.value().toFloat(), 1.0 / tags.length.toDouble)
    }
    pp
  }

  def sentenceToWords(sentence: String): (Array[Int], Array[Int]) = {
    val tokens = sentence.split("\\s+") // the input sentence come pre-tokenized
    val words = new ArrayBuffer[Int]()
    val tags = new ArrayBuffer[Int]()

    for(token <- tokens) {
      words += wordId(token)
      tags += tagId(token)
    }

    (words.toArray, tags.toArray)
  }

  def wordId(word: String): Int = {
    w2i.getOrElse(word, w2i(LstmUtils.UNK_WORD))
  }
  def tagId(word: String): Int = {
    t2i.getOrElse(word, t2i(LstmUtils.UNK_WORD))
  }

  def save(modelFilename: String): Unit = {
    val dynetFilename = mkDynetFilename(modelFilename)
    val x2iFilename = mkX2iFilename(modelFilename)

    new CloseableModelSaver(dynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(parameters, "/lstm")
    }

    Serializer.using(LstmUtils.newPrintWriter(x2iFilename)) { printWriter =>
      val dim = wordLookupParameters.dim().get(0)

      LstmUtils.save(printWriter, w2i, "w2i")
      LstmUtils.save(printWriter, t2i, "t2i")
      LstmUtils.save(printWriter, dim, "dim")
    }
  }
}

object LstmLMTrainer {
  val logger:Logger = LoggerFactory.getLogger(classOf[LstmLMTrainer])

  val MIN_UNK_TAG_FREQ_RATIO  = 0.000001
  val MIN_UNK_WORD_FREQ_RATIO = 0.00000001

  val WORD_EMBEDDING_SIZE = 300
  val WORD_RNN_STATE_SIZE = 1024

  val CLIP_THRESHOLD = 5.0f
  val DROPOUT_PROB:Float = 0.2f
  val BATCH_SIZE = 10

  protected def generateKnownWords(trainFileName: String): (Set[String], Set[String], Int) = {
    logger.debug(s"Counting words in file $trainFileName...")
    val counts = new Counter[String]()
    val source = Source.fromFile(trainFileName)
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
    var totalCounts = 0.0
    for(c <- counts.keySet) {
      totalCounts += counts.getCount(c)
    }
    val knownWords = new mutable.HashSet[String]()
    val tags = new mutable.HashSet[String]()
    for(c <- counts.keySet) {
      val count = counts.getCount(c)
      if(count > totalCounts * MIN_UNK_WORD_FREQ_RATIO) {
        knownWords += c
      }
      if(count > totalCounts * MIN_UNK_TAG_FREQ_RATIO) {
        tags += c
      }
    }
    logger.debug(s"Found ${knownWords.size} not unknown words.")
    logger.debug(s"Found ${tags.size} + 2 tags.")

    // add the virtual words/tags we need
    knownWords += LstmUtils.UNK_WORD
    tags += LstmUtils.UNK_WORD
    tags += LstmUtils.EOS_WORD

    (knownWords.toSet, tags.toSet, sentCount)
  }

  def mkParams(w2i: Map[String, Int], t2i: Map[String, Int]): LstmLMTrainer = {

    val parameters = new ParameterCollection()

    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(WORD_EMBEDDING_SIZE))

    val wordFwBuilder = new LstmBuilder(1, WORD_EMBEDDING_SIZE, WORD_RNN_STATE_SIZE, parameters)
    val wordBwBuilder = new LstmBuilder(1, WORD_EMBEDDING_SIZE, WORD_RNN_STATE_SIZE, parameters)

    val fwO = parameters.addParameters(Dim(t2i.size, WORD_RNN_STATE_SIZE))
    val bwO = parameters.addParameters(Dim(t2i.size, WORD_RNN_STATE_SIZE))

    new LstmLMTrainer(w2i, t2i, parameters,
      wordLookupParameters, wordFwBuilder, wordBwBuilder,
      fwO, bwO)
  }

  def main(args: Array[String]): Unit = {
    initializeDyNet(autoBatch = true, mem = "4096")
    val configName = "lstm-lm"
    val config = new FlairConfig(ConfigFactory.load(configName))

    //
    // test mode
    //
    if(config.contains("lstm.test.model")) {
      logger.debug("Entering evaluation mode...")
    }

    //
    // train mode
    //
    else {
      logger.debug("Entering training mode...")

      // build the set of known characters
      val trainFileName = config.getArgString("lstm.train.train", None)
      val (knownWords, tags, _) = generateKnownWords(trainFileName)
      val w2i = knownWords.toArray.zipWithIndex.toMap
      val t2i = tags.toArray.zipWithIndex.toMap

      val lm = mkParams(w2i, t2i)
      lm.train(
        trainFileName,
        Some(config.getArgString("lstm.train.dev", None)),
        config.getArgInt("lstm.train.logCheckpoint", Some(1000)),
        config.getArgInt("lstm.train.saveCheckpoint", Some(50000))
      )
    }
  }
}
