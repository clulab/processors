package org.clulab.lm

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{ComputationGraph, Dim, Expression, ExpressionVector, LookupParameter, LstmBuilder, Parameter, ParameterCollection, RMSPropTrainer, RnnBuilder}
import org.clulab.sequences.{LstmUtils, SafeTrainer}
import org.clulab.sequences.LstmUtils.{ByLineStringMapBuilder, initializeDyNet, mkDynetFilename, mkX2iFilename}
import org.clulab.struct.Counter
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.io.Source
import LstmLMTrainer._
import edu.cmu.dynet.Expression.{pick, pickNegLogSoftmax, softmax, sum}
import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.utils.Serializer

import scala.collection.mutable.ArrayBuffer
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser

class LstmLMTrainer (val w2i: Map[String, Int],
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
             epochs:Int,
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

    for(epoch <- 0 until epochs) {
      logger.debug(s"Starting epoch #$epoch...")
      for (sentence <- source.getLines()) {
        val (words, chars, tags) = sentenceToWords(sentence)

        //
        // left-to-right prediction
        //
        val fwIn = words
        val fwChars = chars
        val fwTags = tags
        val fwEmissionScores = emissionScoresAsExpressions(fwIn, fwChars, wordFwRnnBuilder, fwO, doDropout = true)
        val fwLoss = languageModelLoss(fwEmissionScores, fwTags)
        batchLosses.add(fwLoss)

        //
        // right-to-left prediction
        //
        val bwIn = words.reverse
        val bwChars = chars.reverse
        val bwTags = tags.reverse
        val bwEmissionScores = emissionScoresAsExpressions(bwIn, bwChars, wordBwRnnBuilder, bwO, doDropout = true)
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
        if (batchLosses.size >= BATCH_SIZE) {
          val comboLoss = sum(batchLosses) / batchLosses.size
          cummulativeLoss += comboLoss.value().toFloat()
          ComputationGraph.backward(comboLoss)
          trainer.update(parameters)

          // report perplexity if a dev file is available
          if (sentCount % saveCheckpoint == 0 && devFileName.nonEmpty) {
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
        if (sentCount % logCheckpoint == 0) {
          logger.debug(s"Processed $sentCount sentences. Cummulative loss: ${cummulativeLoss / numTagged}.")

          // save a model when we hit a save checkpoint
          if (sentCount % saveCheckpoint == 0) {
            val baseModelName = s"lstmlm_s$sentCount"
            save(baseModelName)
          }
        }
      }

      //
      // report perplexity and save a model at the end of each epoch
      //
      logger.debug(s"Epoch #$epoch complete.")
      logger.debug(s"Processed $sentCount sentences at the end of epoch $epoch. Cummulative loss: ${cummulativeLoss / numTagged}.")
      if(devFileName.nonEmpty) {
        reportPerplexity(devFileName.get)
      }
      val baseModelName = s"lstmlm_epoch$epoch"
      save(baseModelName)
    }
  }

  def emissionScoresAsExpressions(words: Array[Int],
                                  chars: Array[Array[Int]],
                                  rnnBuilder: RnnBuilder,
                                  pO:Parameter,
                                  doDropout:Boolean = false): ExpressionVector = {
    if(doDropout) {
      rnnBuilder.setDropout(DROPOUT_PROB)
      charFwRnnBuilder.setDropout(DROPOUT_PROB)
      charBwRnnBuilder.setDropout(DROPOUT_PROB)
    } else {
      rnnBuilder.disableDropout()
      charFwRnnBuilder.disableDropout()
      charBwRnnBuilder.disableDropout()
    }

    val embeddings = words.zip(chars).map(mkEmbedding)
    val states = LstmUtils.transduce(embeddings, rnnBuilder)

    val O = Expression.parameter(pO)
    val emissionScores = new ExpressionVector()
    for(s <- states) {
      emissionScores.add(O * s)
    }

    emissionScores
  }

  def mkEmbedding(wordAndCharIds: Tuple2[Int, Array[Int]]): Expression = {
    val wid = wordAndCharIds._1
    val cids = wordAndCharIds._2

    val wordEmbedding = Expression.lookup(wordLookupParameters, wid)
    val charEmbedding = mkCharacterEmbedding(cids)

    Expression.concatenate(wordEmbedding, charEmbedding)
  }

  /** BiLSTM over character embeddings */
  private def mkCharacterEmbedding(cids: Array[Int]): Expression = {
    val charEmbeddings = new ArrayBuffer[Expression]()
    for(i <- cids.indices) {
      charEmbeddings += Expression.lookup(charLookupParameters, cids(i))
    }
    val fwOut = LstmUtils.transduce(charEmbeddings, charFwRnnBuilder).last
    val bwOut = LstmUtils.transduce(charEmbeddings.reverse, charBwRnnBuilder).last
    Expression.concatenate(fwOut, bwOut)
  }

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
      val (words, chars, tags) = sentenceToWords(sentence)
      ComputationGraph.renew()

      val fwIn = words
      val fwChars = chars
      val fwTags = tags
      val fwEmissionScores = emissionScoresAsExpressions(fwIn, fwChars, wordFwRnnBuilder, fwO) // no dropout during testing!
      val fwPp = perplexity(fwEmissionScores, fwTags)

      val bwIn = words.reverse
      val bwChars = chars.reverse
      val bwTags = tags.reverse
      val bwEmissionScores = emissionScoresAsExpressions(bwIn, bwChars, wordBwRnnBuilder, bwO)
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

  def sentenceToWords(sentence: String): (Array[Int], Array[Array[Int]], Array[Int]) = {
    val tokens = sentence.split("\\s+") // the input sentence come pre-tokenized
    val words = new ArrayBuffer[Int]()
    val chars = new ArrayBuffer[Array[Int]]()
    val tags = new ArrayBuffer[Int]()

    for(token <- tokens) {
      words += wordId(token)

      val charIds = new ArrayBuffer[Int]()
      for(i <- token.indices) {
        val cid = c2i.getOrElse(token.charAt(i), 0) // id 0 is reserved for unknown chars
        charIds += cid
      }
      chars += charIds.toArray

      tags += tagId(token)
    }

    (words.toArray, chars.toArray, tags.toArray)
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

    logger.debug(s"Saving parameters to file $dynetFilename...")
    new CloseableModelSaver(dynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(parameters, "/lstm")
    }

    logger.debug(s"Saving x2i information to file $x2iFilename...")
    Serializer.using(LstmUtils.newPrintWriter(x2iFilename)) { printWriter =>
      LstmUtils.saveCharMap(printWriter, c2i, "c2i")
      LstmUtils.save(printWriter, w2i, "w2i")
      LstmUtils.save(printWriter, t2i, "t2i")
      LstmUtils.save(printWriter, wordLookupParameters.dim().get(0), "wordDim")
      LstmUtils.save(printWriter, charLookupParameters.dim().get(0), "charDim")
      LstmUtils.save(printWriter, CHAR_RNN_STATE_SIZE, "charRnnDim")
      LstmUtils.save(printWriter, RNN_STATE_SIZE, "rnnDim")
    }
  }
}

object LstmLMTrainer {
  val logger:Logger = LoggerFactory.getLogger(classOf[LstmLMTrainer])

  val MIN_UNK_TAG_FREQ_RATIO  = 0.000001
  val MIN_UNK_WORD_FREQ_RATIO = 0.00000001

  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16

  val RNN_STATE_SIZE = 256 // 1024 // TODO

  val CLIP_THRESHOLD = 5.0f
  val DROPOUT_PROB:Float = 0.2f
  val BATCH_SIZE = 1 // 10 // TODO?

  protected def loadMandatoryWords(mandatoryWordsFile: String,
                                   caseInsensitive: Boolean,
                                   knownWords:mutable.HashSet[String]): Unit = {
    val source = Source.fromFile(mandatoryWordsFile)
    var wordCount = 0
    for(line <- source.getLines()) {
      val word = line.trim
      val key = if(caseInsensitive) word.toLowerCase() else word
      knownWords += key
      wordCount += 1
    }
    source.close()
    logger.debug(s"Using $wordCount mandatory words.")
  }

  protected def generateKnownWords(trainFileName: String,
                                   caseInsensitiveKnownWords: Boolean,
                                   knownWords: mutable.HashSet[String],
                                   knownTags: mutable.HashSet[String]) {

    logger.debug(s"Counting words in file $trainFileName...")
    val counts = new Counter[String]()
    val source = Source.fromFile(trainFileName)
    var sentCount = 0
    for(line <- source.getLines()) {
      val tokens = line.split("\\s+")
      for(word <- tokens) {
        val key = if(caseInsensitiveKnownWords) word.toLowerCase() else word
        counts.incrementCount(key)
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

    for(c <- counts.keySet) {
      val count = counts.getCount(c)
      if(count > totalCounts * MIN_UNK_WORD_FREQ_RATIO) {
        knownWords += c
      }
      if(count > totalCounts * MIN_UNK_TAG_FREQ_RATIO) {
        knownTags += c
      }
    }
    logger.debug(s"Found ${knownWords.size} + 1 known words.")
    logger.debug(s"Found ${knownTags.size} + 2 tags.")

  }

  def loadEmbeddings(embeddingsFile:String,
                     knownWords:Set[String],
                     caseInsensitive:Boolean): Word2Vec = {
    logger.debug(s"Loading embeddings from file $embeddingsFile...")
    val w2v = new Word2Vec(embeddingsFile, Some(knownWords), caseInsensitiveWordsToUse = caseInsensitive)
    logger.debug(s"Completed loading embeddings for a vocabulary of size ${w2v.matrix.size}.")

    w2v
  }

  def loadWordsToUse(docFreqFileName: String, minDocFreq: Int, knownWords: mutable.HashSet[String]) {
    logger.debug(s"Loading words to use from file $docFreqFileName using min frequency of $minDocFreq.")
    val source = Source.fromFile(docFreqFileName)
    var total = 0
    var kept = 0
    for(line <- source.getLines()) {
      total += 1
      val tokens = line.split("\\s+")
      // println(s"Reading line: ${tokens.mkString(", ")}")
      assert(tokens.length == 2)
      if(tokens(1).toInt > minDocFreq) {
        kept += 1
        knownWords += tokens(0)
      }
    }
    source.close()
    logger.debug(s"Loaded $kept words to use, from a total of $total words.")
  }

  def apply(baseModelFilename: String): LstmLMTrainer = {
    load(baseModelFilename)
  }

  def load(baseModelFilename: String): LstmLMTrainer = {
    logger.debug(s"Loading LstmLM model from $baseModelFilename...")
    val dynetFilename = mkDynetFilename(baseModelFilename)
    val x2iFilename = mkX2iFilename(baseModelFilename)

    //
    // load the .x2i info
    //
    val (w2i, t2i, c2i, wordDim, charDim, charRnnDim, rnnDim) = Serializer.using(LstmUtils.newSource(x2iFilename)) { source =>
      val byLineStringMapBuilder = new ByLineStringMapBuilder()
      val byLineCharMapBuilder = new LstmUtils.ByLineCharIntMapBuilder()
      val byLineIntBuilder = new LstmUtils.ByLineIntBuilder()
      val lines = source.getLines()
      val c2i = byLineCharMapBuilder.build(lines)
      val w2i = byLineStringMapBuilder.build(lines)
      val t2i = byLineStringMapBuilder.build(lines)
      val wordDim = byLineIntBuilder.build(lines)
      val charDim = byLineIntBuilder.build(lines)
      val charRnnDim = byLineIntBuilder.build(lines)
      val rnnDim = byLineIntBuilder.build(lines)
      (w2i, t2i, c2i, wordDim, charDim, charRnnDim, rnnDim)
    }
    logger.debug(s"Loaded a Lstm LM word map with ${w2i.keySet.size} words and ${t2i.keySet.size} labels.")
    logger.debug(s"Loaded a character map with ${c2i.keySet.size} known characters.")

    //
    // create the DyNet parameters
    //
    val model = mkParams(w2i, t2i, c2i, wordDim, charDim, charRnnDim, rnnDim, None)

    //
    // load the above parameters from the DyNet model file
    //
    LstmUtils.loadParameters(dynetFilename, model.parameters, key = "/lstm")

    model
  }

  def mkParams(w2i: Map[String, Int],
               t2i: Map[String, Int],
               c2i: Map[Char, Int],
               wordDim: Int,
               charDim: Int,
               charRnnDim: Int,
               rnnDim: Int,
               w2v: Option[Word2Vec]): LstmLMTrainer = {

    val parameters = new ParameterCollection()

    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(wordDim))
    val wordFwBuilder = new LstmBuilder(1, wordDim + 2 * charRnnDim, rnnDim, parameters)
    val wordBwBuilder = new LstmBuilder(1, wordDim + 2 * charRnnDim, rnnDim, parameters)

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(charDim))
    val charFwRnnBuilder = new LstmBuilder(1, charDim, charRnnDim, parameters)
    val charBwRnnBuilder = new LstmBuilder(1, charDim, charRnnDim, parameters)

    val fwO = parameters.addParameters(Dim(t2i.size, rnnDim))
    val bwO = parameters.addParameters(Dim(t2i.size, rnnDim))
    logger.debug("Made all parameters.")

    new LstmLMTrainer(w2i, t2i, c2i, parameters,
      wordLookupParameters, wordFwBuilder, wordBwBuilder,
      charLookupParameters, charFwRnnBuilder, charBwRnnBuilder,
      fwO, bwO)
  }

  def main(args: Array[String]): Unit = {
    initializeDyNet(autoBatch = true, mem = "1660,1664,2496,1400")
    val configName = "lstm-lm"
    val config = new FlairConfig(ConfigFactory.load(configName))

    //
    // test mode
    //
    if(config.contains("lstm.test.model")) {
      logger.debug("Entering evaluation mode...")
      // TODO: compute perplexity on the test dataset
    }

    //
    // train mode
    //
    else {
      logger.debug("Entering training mode...")

      //
      // build the set of known words
      //
      val knownWords = new mutable.HashSet[String]()
      val knownTags = new mutable.HashSet[String]()
      val caseInsensitiveKnownWords = true

      if(config.contains("lstm.train.mandatoryWords")) {
        loadMandatoryWords(
          config.getArgString("lstm.train.mandatoryWords", None),
          caseInsensitiveKnownWords,
          knownWords)
      }

      val trainFileName = config.getArgString("lstm.train.train", None)
      /*
      generateKnownWords(
        trainFileName, caseInsensitiveKnownWords,
        knownWords, knownTags
      )
      */

      val embedFilename = config.getArgString("lstm.train.embed", None)
      val docFreqFilename = config.getArgString("lstm.train.docFreq", None)
      val minFreq = config.getArgInt("lstm.train.minWordFreq", Some(100))
      val w2v = LstmUtils.loadEmbeddings(Some(docFreqFilename), minFreq, embedFilename,
        Some(config.getArgString("lstm.train.mandatoryWords", None)))
      val w2i = LstmUtils.mkWordVocab(w2v)

      /*
      val docFreqFilename = config.getArgString("lstm.train.docFreq", None)
      val minFreq = config.getArgInt("lst.train.minWordFreq", Some(100))
      loadWordsToUse(docFreqFilename, minFreq, knownWords)

      logger.debug(s"knownWords has size ${knownWords.size}.")
      logger.debug(s"knownTags has size ${knownTags.size}.")

      //
      // load input embeddings
      //
      val w2v = loadEmbeddings(
        config.getArgString("lstm.train.embed", None),
        knownWords.toSet,
        caseInsensitiveKnownWords
      )

      //val w2i = knownWords.toArray.zipWithIndex.toMap
      val w2i = LstmUtils.mkWordVocab(w2v) // this adds UNK to w2i
      */

      knownTags += LstmUtils.UNK_WORD
      knownTags += LstmUtils.EOS_WORD
      val t2i = knownTags.toArray.zipWithIndex.toMap

      logger.debug(s"w2i has size ${w2i.size}.")
      logger.debug(s"t2i has size ${t2i.size}.")

      //
      // load the character map
      //
      logger.debug(s"Loading the character map...")
      val c2iFilename = config.getArgString("lstm.train.c2i", None)
      val c2i = Serializer.using(LstmUtils.newSource(c2iFilename)) { source =>
        val byLineCharMapBuilder = new LstmUtils.ByLineCharIntMapBuilder()
        val lines = source.getLines()
        val c2i = byLineCharMapBuilder.build(lines)
        c2i
      }
      logger.debug(s"Loaded a character map with ${c2i.keySet.size} entries. ")

      val lm = mkParams(w2i, t2i, c2i,
        w2v.dimensions, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, RNN_STATE_SIZE,
        Some(w2v))

      /*
      lm.train(
        trainFileName,
        Some(config.getArgString("lstm.train.dev", None)),
        config.getArgInt("lstm.train.epochs", Some(1)),
        config.getArgInt("lstm.train.logCheckpoint", Some(1000)),
        config.getArgInt("lstm.train.saveCheckpoint", Some(50000))
      )
      */
      lm.save("lstmlm_final")
      logger.info("Success.")
    }
  }
}
