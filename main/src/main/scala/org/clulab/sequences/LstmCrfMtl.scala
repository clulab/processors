package org.clulab.sequences

import com.typesafe.config.ConfigFactory
import org.slf4j.{Logger, LoggerFactory}
import edu.cmu.dynet.{Dim, FloatVector, Initialize, LookupParameter, LstmBuilder, Parameter, ParameterCollection, RnnBuilder}
import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.struct.Counter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import edu.cmu.dynet.Expression.randomNormal
import org.clulab.sequences.ArrayMath.toFloatArray
import org.clulab.sequences.LstmCrf.logger

import LstmUtils._
import LstmCrfMtl._

/**
  * Implements a multi-task learning (MTL) framework around the biLSTM-CRF of Lample et al. (2016)
  * We use GloVe embeddings and learned character embeddings
  * @author Mihai
  */
class LstmCrfMtl(val taskManager: TaskManager) {
  /** Stores the DyNet parameters required by all tasks */
  var model:LstmCrfMtlParameters = _

  def initialize():Unit = {
    val w2v = loadEmbeddings(Some(taskManager.docFreqFileName), taskManager.minWordFreq, taskManager.embeddingsFileName)

    val (w2i, c2i, t2is) = mkVocabs(w2v)
    logger.debug(s"Word vocabulary has ${w2i.size} entries (including 1 for unknown).")
    logger.debug(s"Character vocabulary has ${c2i.size} entries.")
    logger.debug(s"Tag vocabulary has:")
    for(i <- t2is.indices) {
      logger.debug(s"- ${t2is(i).size} entries for task ${taskManager.tasks(i).taskNumber}")
    }

    logger.debug("Initializing DyNet...")
    Initialize.initialize(Map("random-seed" -> RANDOM_SEED))
    model = mkParams(w2i, c2i, t2is, w2v.dimensions)

    model.initializeEmbeddings(w2v)
    model.initializeTransitions()

    logger.debug("Completed initialization.")
  }

  def mkParams(w2i:Map[String, Int],
               c2i:Map[Char, Int],
               t2is:Array[Map[String, Int]],
               embeddingDim:Int): LstmCrfMtlParameters = {
    val parameters = new ParameterCollection()

    // These parameters correspond to the LSTM(s) shared by all tasks
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(embeddingDim))
    val embeddingSize = embeddingDim + 2 * CHAR_RNN_STATE_SIZE
    val fwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)
    val bwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)

    // These parameters are unique for each task
    val Hs = new Array[Parameter](taskManager.taskCount)
    val Os = new Array[Parameter](taskManager.taskCount)
    val i2ts = new Array[Array[String]](taskManager.taskCount)
    val Ts = new Array[LookupParameter](taskManager.taskCount)
    for(tid <- taskManager.indices) {
      Hs(tid) = parameters.addParameters(Dim(NONLINEAR_SIZE, 2 * RNN_STATE_SIZE))
      Os(tid) = parameters.addParameters(Dim(t2is(tid).size, NONLINEAR_SIZE))
      i2ts(tid) = fromIndexToString(t2is(tid))
      Ts(tid) = mkTransitionMatrix(parameters, t2is(tid), i2ts(tid))
    }
    logger.debug("Created parameters.")

    new LstmCrfMtlParameters(w2i, c2i, t2is, i2ts,
      parameters, lookupParameters,
      fwBuilder, bwBuilder,
      charLookupParameters, charFwBuilder, charBwBuilder,
      Hs, Os, Ts)
  }

  private def mkVocabs(w2v:Word2Vec): (Map[String, Int], Map[Char, Int], Array[Map[String, Int]]) = {
    val tags = new Array[Counter[String]](taskManager.taskCount)
    for(i <- tags.indices) tags(i) = new Counter[String]()
    val chars = new mutable.HashSet[Char]()

    for(tid <- taskManager.indices) {
      for (sentence <- taskManager.tasks(tid).trainSentences) {
        for (token <- sentence) {
          val word = token.getWord
          for (i <- word.indices) {
            chars += word.charAt(i)
          }
          tags(tid) += token.getTag
        }
      }
    }

    val commonWords = new ListBuffer[String]
    commonWords += UNK_WORD // the word at position 0 is reserved for unknown words
    for(w <- w2v.matrix.keySet.toList.sorted) {
      commonWords += w
    }

    for(tid <- tags.indices) {
      tags(tid) += START_TAG
      tags(tid) += STOP_TAG
    }

    val w2i = commonWords.zipWithIndex.toMap
    val c2i = chars.toList.sorted.zipWithIndex.toMap
    val t2is = new Array[Map[String, Int]](taskManager.taskCount)
    for(tid <- taskManager.indices) {
      t2is(tid) = tags(tid).keySet.toList.sorted.zipWithIndex.toMap
    }

    (w2i, c2i, t2is)
  }

  def train():Unit = {
    // TODO
  }
}

class LstmCrfMtlParameters(
  var w2i:Map[String, Int],
  val c2i:Map[Char, Int],
  val t2is:Array[Map[String, Int]],
  val i2ts:Array[Array[String]],
  val parameters:ParameterCollection,
  var lookupParameters:LookupParameter,
  val fwRnnBuilder:RnnBuilder,
  val bwRnnBuilder:RnnBuilder,
  val charLookupParameters:LookupParameter,
  val charFwRnnBuilder:RnnBuilder,
  val charBwRnnBuilder:RnnBuilder,
  val Hs:Array[Parameter],
  val Os:Array[Parameter],
  val Ts:Array[LookupParameter]) { // transition matrix for Viterbi; T[i][j] = transition *to* i *from* j

  def taskCount:Int = t2is.length
  def indices:Range = t2is.indices

  def initializeEmbeddings(w2v:Word2Vec): Unit = {
    logger.debug("Initializing DyNet embedding parameters...")
    for(word <- w2v.matrix.keySet){
      lookupParameters.initialize(w2i(word), new FloatVector(toFloatArray(w2v.matrix(word))))
    }
    logger.debug(s"Completed initializing embedding parameters for a vocabulary of size ${w2v.matrix.size}.")
  }

  def initializeTransitions(): Unit = {
    // needs to be done separately for each task
    for(tid <- indices) {
      val startTag = t2is(tid)(START_TAG)
      val stopTag = t2is(tid)(STOP_TAG)

      for (i <- 0 until t2is(tid).size) {
        Ts(tid).initialize(i, initTransitionsTo(tid, i, t2is(tid).size, startTag, stopTag))
      }
    }
  }

  def initTransitionsTo(tid:Int, dst: Int, size:Int, startTag: Int, stopTag: Int): FloatVector = {
    val transScores = new Array[Float](size)

    for(i <- 0 until size) {
      transScores(i) = randomNormal(Dim(1)).value().toFloat() / size // pseudo Glorot
    }

    if(LstmCrfMtl.USE_DOMAIN_CONSTRAINTS) {
      // discourage transitions to START from anything
      if (dst == startTag) {
        for (i <- 0 until size)
          transScores(i) = LOG_MIN_VALUE
      } else {
        // discourage transitions to anything from STOP
        transScores(stopTag) = LOG_MIN_VALUE

        // discourage transitions to I-X from B-Y or I-Y
        val dstTag = i2ts(tid)(dst)
        if (dstTag.startsWith("I-")) {
          for (i <- 0 until size) {
            val srcTag = i2ts(tid)(i)
            if ((srcTag.startsWith("B-") || srcTag.startsWith("I-")) &&
              srcTag.substring(2) != dstTag.substring(2)) {
              transScores(i) = LOG_MIN_VALUE
            }
          }
        }
      }
    }

    new FloatVector(transScores)
  }
}

object LstmCrfMtl {
  val logger:Logger = LoggerFactory.getLogger(classOf[LstmCrfMtl])

  val RNN_STATE_SIZE = 50
  val NONLINEAR_SIZE = 32
  val RNN_LAYERS = 1
  val CHAR_RNN_LAYERS = 1
  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16

  /** Use domain constraints in the transition probabilities? */
  val USE_DOMAIN_CONSTRAINTS = true

  def main(args: Array[String]): Unit = {
    //
    // to set a custom config file add -Dconfig.file=/path/to/conf/file to the cmd line for sbt
    //
    val config = ConfigFactory.load()

    val taskManager = new TaskManager(config)
    val mtl = new LstmCrfMtl(taskManager)
    mtl.initialize()

    mtl.train()
  }
}