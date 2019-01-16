package org.clulab.sequences

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.struct.Counter
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import edu.cmu.dynet._
import edu.cmu.dynet.Expression._
import RNN._

import scala.collection.mutable

class RNN {
  var model:RNNParameters = _

  def train(trainSentences:Array[Array[Row]], devSentences:Array[Array[Row]], embeddingsFile:String): Unit = {
    val (w2i, t2i, c2i) = mkVocabs(trainSentences)
    logger.debug(s"Tag vocabulary has ${t2i.size} entries.")
    logger.debug(s"Word vocabulary has ${w2i.size} entries (including 1 for unknown).")
    logger.debug(s"Character vocabulary has ${c2i.size} entries.")

    initialize(w2i, t2i, c2i, embeddingsFile)
    update(trainSentences:Array[Array[Row]], devSentences:Array[Array[Row]])
  }

  def update(trainSentences: Array[Array[Row]], devSentences:Array[Array[Row]]): Unit = {
    //val trainer = new SimpleSGDTrainer(model.parameters, learningRate = 0.01f)
    val trainer = new RMSPropTrainer(model.parameters)
    var cummulativeLoss = 0.0
    var numTagged = 0
    var sentCount = 0
    for(epoch <- 0 until EPOCHS) {
      logger.info(s"Started epoch $epoch.")
      for(sentence <- trainSentences) {
        sentCount += 1
        //logger.debug(s"Predicting sentence $sentCount: " + sentence.map(_.get(0)).mkString(", "))

        // predict probabilities for one sentence
        val words = sentence.map(_.get(0))
        val probs = predictSentence(words)
        //for (prob <- probs) logger.debug("Probs: " + prob.value().toVector())

        // compute loss for this sentence
        val loss = sentenceLoss(sentence.map(_.get(1)), probs)

        cummulativeLoss += loss.value().toFloat
        numTagged += sentence.length

        if(sentCount % 1000 == 0) {
          logger.info("Cummulative loss: " + cummulativeLoss / numTagged)
          cummulativeLoss = 0.0
          numTagged = 0
        }

        // backprop
        ComputationGraph.backward(loss)
        trainer.update()
      }

      dev(devSentences)
    }
  }

  def dev(devSentences:Array[Array[Row]]): Unit = {
    var total = 0
    var correct = 0

    logger.debug("Started evaluation on dev...")
    for(sent <- devSentences) {
      val words = sent.map(_.get(0))
      val golds = sent.map(_.get(1))

      val preds = predict(words)
      assert(golds.length == preds.length)
      total += golds.length
      for(e <- preds.zip(golds)) {
        if(e._1 == e._2) {
          correct += 1
        }
      }
    }

    logger.info(s"Accuracy on ${devSentences.length} dev sentences: " + correct.toDouble / total)
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
    * @param words One training or testing sentence
    */
  def predictSentence(words: Array[String]): Iterable[Expression] = {
    ComputationGraph.renew()

    val embeddings = words.map(mkEmbedding)

    val fwStates = transduce(embeddings, model.fwRnnBuilder)
    val bwStates = transduce(embeddings.reverse, model.bwRnnBuilder).toArray.reverse
    assert(fwStates.size == bwStates.length)
    val states = concatenateStates(fwStates, bwStates)

    val H = parameter(model.H)
    val O = parameter(model.O)

    states.map(s => O * Expression.tanh(H * s))
  }

  def predict(words:Array[String]):Array[String] = synchronized {
    val scores = predictSentence(words)
    val tags = new ArrayBuffer[String]()
    for(score <- scores) {
      val probs = softmax(score).value().toVector().toArray
      var max = Float.MinValue
      var tid = -1
      for(i <- probs.indices) {
        if(probs(i) > max) {
          max = probs(i)
          tid = i
        }
      }
      assert(tid > -1)
      tags += model.i2t(tid)
    }

    tags.toArray
  }

  def concatenateStates(l1: Iterable[Expression], l2: Iterable[Expression]): Iterable[Expression] = {
    val c = new ArrayBuffer[Expression]()
    for(e <- l1.zip(l2)) {
      c += concatenate(e._1, e._2)
    }
    c
  }

  def mkEmbedding(word: String):Expression = {
    val sanitized = Word2Vec.sanitizeWord(word)

    val wordEmbedding =
      if(model.w2i.contains(sanitized))
      // found the word in the known vocabulary
        lookup(model.lookupParameters, model.w2i(sanitized))
      else
      // not found; return the embedding at position 0, which is reserved for unknown words
        lookup(model.lookupParameters, 0)

    // biLSTM over character embeddings
    val charEmbedding =
      mkCharacterEmbedding(word)

    concatenate(wordEmbedding, charEmbedding)
  }

  def mkCharacterEmbedding(word: String): Expression = {
    val charEmbeddings = word.toCharArray.map(lookup(model.charLookupParameters, _))
    val fwOut = transduce(charEmbeddings, model.charFwRnnBuilder).last
    val bwOut = transduce(charEmbeddings.reverse, model.charBwRnnBuilder).last
    concatenate(fwOut, bwOut)
  }

  def transduce(embeddings:Iterable[Expression], builder:RnnBuilder): Iterable[Expression] = {
    builder.newGraph()
    builder.startNewSequence()
    val states = embeddings.map(builder.addInput)
    states
  }

  def initialize(w2i:Map[String, Int], t2i:Map[String, Int], c2i:Map[Character, Int], embeddingsFile:String): Unit = {
    logger.debug("Initializing DyNet...")
    Initialize.initialize(Map("random-seed" -> RANDOM_SEED))
    model = mkParams(w2i, t2i, c2i, embeddingsFile)
    logger.debug("Completed initialization.")
  }

  def mkParams(w2i:Map[String, Int], t2i:Map[String, Int], c2i:Map[Character, Int], embeddingsFile:String): RNNParameters = {
    val parameters = new ParameterCollection()
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(EMBEDDING_SIZE))
    val fwBuilder = new LstmBuilder(RNN_LAYERS, EMBEDDING_SIZE + 2 * CHAR_RNN_STATE_SIZE, RNN_STATE_SIZE, parameters)
    val bwBuilder = new LstmBuilder(RNN_LAYERS, EMBEDDING_SIZE + 2 * CHAR_RNN_STATE_SIZE, RNN_STATE_SIZE, parameters)
    val H = parameters.addParameters(Dim(NONLINEAR_SIZE, 2 * RNN_STATE_SIZE))
    val O = parameters.addParameters(Dim(t2i.size, NONLINEAR_SIZE))
    val i2t = fromIndexToString(t2i)
    logger.debug("Created parameters.")

    logger.debug(s"Loading embeddings from file $embeddingsFile...")
    val w2v = new Word2Vec(embeddingsFile, Some(w2i.keySet))
    for(word <- w2i.keySet) {
      if(w2v.matrix.contains(word)) {
        lookupParameters.initialize(w2i(word), new FloatVector(toFloatArray(w2v.matrix(word))))
      }
    }
    logger.debug(s"Loaded ${w2v.matrix.size} embeddings.")

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)

    new RNNParameters(w2i, t2i, i2t, c2i, parameters, lookupParameters, fwBuilder, bwBuilder, H, O,
      charLookupParameters, charFwBuilder, charBwBuilder)
  }

  def toFloatArray(doubles:Array[Double]): Array[Float] = {
    val floats = new Array[Float](doubles.length)
    for(i <- doubles.indices) {
      floats(i) = doubles(i).toFloat
    }
    floats
  }

  def fromIndexToString(s2i: Map[String, Int]):Map[Int, String] = {
    val i2s = new mutable.HashMap[Int, String]()
    for(k <- s2i.keySet) {
      i2s += (s2i(k) -> k)
    }
    i2s.toMap
  }

  def mkVocabs(trainSentences:Array[Array[Row]]): (Map[String, Int], Map[String, Int], Map[Character, Int]) = {
    val words = new Counter[String]()
    val tags = new Counter[String]()
    val chars = new mutable.HashSet[Character]()
    for(sentence <- trainSentences) {
      for(token <- sentence) {
        val word = token.get(0)
        words += Word2Vec.sanitizeWord(word)
        for(i <- word.indices) {
          chars += word.charAt(i)
        }
        tags += token.get(1)
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
    val c2i = chars.toList.zipWithIndex.toMap

    (w2i, t2i, c2i)
  }

}

class RNNParameters(
  val w2i:Map[String, Int],
  val t2i:Map[String, Int],
  val i2t:Map[Int, String],
  val c2i:Map[Character, Int],
  val parameters:ParameterCollection,
  val lookupParameters:LookupParameter,
  val fwRnnBuilder:RnnBuilder,
  val bwRnnBuilder:RnnBuilder,
  val H:Parameter,
  val O:Parameter,
  val charLookupParameters:LookupParameter,
  val charFwRnnBuilder:RnnBuilder,
  val charBwRnnBuilder:RnnBuilder
)

object RNN {
  val logger:Logger = LoggerFactory.getLogger(classOf[RNN])

  val EPOCHS = 50
  val RANDOM_SEED = 2522620396l
  val EMBEDDING_SIZE = 200
  val RNN_STATE_SIZE = 50
  val NONLINEAR_SIZE = 32
  val RNN_LAYERS = 1
  val CHAR_RNN_LAYERS = 1
  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16

  def main(args: Array[String]): Unit = {
    val trainFile = args(0)
    val devFile = args(1)
    val trainSentences = ColumnReader.readColumns(trainFile)
    val devSentences = ColumnReader.readColumns(devFile)
    val embeddingsFile = args(2)

    val rnn = new RNN()
    rnn.train(trainSentences, devSentences, embeddingsFile)
  }
}