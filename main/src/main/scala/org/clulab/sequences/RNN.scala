package org.clulab.sequences

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.struct.Counter
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import edu.cmu.dynet._
import edu.cmu.dynet.Expression._
import RNN._

class RNN {
  var model:RNNParameters = _

  def train(trainSentences:Array[Array[Row]]): Unit = {
    val (w2i, t2i) = mkVocabs(trainSentences)

    logger.debug(s"Tag vocabulary has ${t2i.size} entries.")
    logger.debug(s"Word vocabulary has ${w2i.size} entries (including 1 for unknown).")

    initialize(w2i, t2i)
    update(trainSentences:Array[Array[Row]])
  }

  def update(trainSentences: Array[Array[Row]]): Unit = {
    val trainer = new SimpleSGDTrainer(model.parameters)
    var cummulativeLoss = 0.0
    var numTagged = 0
    var sentCount = 0
    for(epoch <- 0 until EPOCHS) {
      logger.debug(s"Started epoch $epoch.")
      for(sentence <- trainSentences) {
        sentCount += 1
        //logger.debug(s"Predicting sentence $sentCount: " + sentence.map(_.get(0)).mkString(", "))

        // predict probabilities for one sentence
        val probs = predictSentence(sentence)
        //for (prob <- probs) logger.debug("Probs: " + prob.value().toVector())

        // compute loss for this sentence
        val loss = sentenceLoss(sentence.map(_.get(1)), probs)

        cummulativeLoss += loss.value().toFloat
        numTagged += sentence.length

        if(sentCount % 10 == 0) {
          logger.info("Cummulative loss: " + cummulativeLoss / numTagged)
          cummulativeLoss = 0.0
          numTagged = 0
        }

        // backprop
        ComputationGraph.backward(loss)
        trainer.update()
      }
    }
  }

  def sentenceLoss(tags:Iterable[String], probs:Iterable[Expression]): Expression = {
    val losses = new ExpressionVector()
    for(e <- tags.zip(probs)) {
      val t = e._1
      val prob = e._2
      val tid = model.t2i(t)
      val loss = pickNegLogSoftmax(prob, tid)
      losses.add(loss)
    }
    Expression.sum(losses)
  }

  /**
    * Generates tag probabilities for the words in this sequence
    * @param sentence One training or testing sentence
    */
  def predictSentence(sentence: Array[Row]): Iterable[Expression] = {
    ComputationGraph.renew()

    val words = sentence.map(_.get(0))
    val embeddings = words.map(mkEmbedding)

    val fwStates = transduce(embeddings, model.fwRnnBuilder)
    val bwStates = transduce(embeddings.reverse, model.bwRnnBuilder).toArray.reverse
    assert(fwStates.size == bwStates.length)
    val states = concantenate(fwStates, bwStates)

    val H = parameter(model.H)
    val O = parameter(model.O)

    states.map(s => softmax(O * tanh(H * s)))
  }

  def concantenate(l1: Iterable[Expression], l2: Iterable[Expression]): Iterable[Expression] = {
    val c = new ArrayBuffer[Expression]()
    for(e <- l1.zip(l2)) {
      c += concatenate(e._1, e._2)
    }
    c
  }

  def mkEmbedding(word: String):Expression = {
    val sanitized = Word2Vec.sanitizeWord(word)
    if(model.w2i.contains(sanitized))
      // found the word in the known vocabulary
      lookup(model.lookupParameters, model.w2i(sanitized))
    else
      // not found; return the embedding at position 0, which is reserved for unknown words
      lookup(model.lookupParameters, 0)
  }

  def transduce(embeddings:Iterable[Expression], builder:RnnBuilder): Iterable[Expression] = {
    builder.newGraph()
    builder.startNewSequence()
    val states = embeddings.map(builder.addInput)
    states
  }

  def initialize(w2i:Map[String, Int], t2i:Map[String, Int]): Unit = {
    logger.debug("Initializing DyNet...")
    Initialize.initialize(Map("random-seed" -> RANDOM_SEED))
    model = mkParams(w2i, t2i)
    logger.debug("Completed initialization.")
  }

  def mkParams(w2i:Map[String, Int], t2i:Map[String, Int]): RNNParameters = {
    val parameters = new ParameterCollection()
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(EMBEDDING_SIZE))
    val fwBuilder = new LstmBuilder(RNN_LAYERS, EMBEDDING_SIZE, RNN_STATE_SIZE, parameters)
    val bwBuilder = new LstmBuilder(RNN_LAYERS, EMBEDDING_SIZE, RNN_STATE_SIZE, parameters)
    val H = parameters.addParameters(Dim(NONLINEAR_SIZE, 2 * RNN_STATE_SIZE))
    val O = parameters.addParameters(Dim(t2i.size, NONLINEAR_SIZE))
    logger.debug("Created parameters.")

    new RNNParameters(w2i, t2i, parameters, lookupParameters, fwBuilder, bwBuilder, H, O)
  }

  def mkVocabs(trainSentences:Array[Array[Row]]): (Map[String, Int], Map[String, Int]) = {
    val words = new Counter[String]()
    val tags = new Counter[String]()
    for(sentence <- trainSentences) {
      for(word <- sentence) {
        words += Word2Vec.sanitizeWord(word.get(0))
        tags += word.get(1)
      }
    }

    val commonWords = new ListBuffer[String]
    commonWords += "*unknown*" // first position reserved for the unknown token
    for(w <- words.keySet) {
      if(words.getCount(w) > 1) {
        commonWords += w
      }
    }

    val w2i = commonWords.zipWithIndex.toMap
    val t2i = tags.keySet.toList.zipWithIndex.toMap

    (w2i, t2i)
  }

}

class RNNParameters(
  val w2i:Map[String, Int],
  val t2i:Map[String, Int],
  val parameters:ParameterCollection,
  val lookupParameters:LookupParameter,
  val fwRnnBuilder:RnnBuilder,
  val bwRnnBuilder:RnnBuilder,
  val H:Parameter,
  val O:Parameter
)

object RNN {
  val logger:Logger = LoggerFactory.getLogger(classOf[RNN])

  val EPOCHS = 1
  val RANDOM_SEED = 2522620396l
  val EMBEDDING_SIZE = 200
  val RNN_STATE_SIZE = 50
  val NONLINEAR_SIZE = 32
  val RNN_LAYERS = 1

  def main(args: Array[String]): Unit = {
    val trainFile = args(0)
    val trainSentences = ColumnReader.readColumns(trainFile)

    val rnn = new RNN()
    rnn.train(trainSentences)
  }
}