package org.clulab.sequences

import java.io.{FileWriter, PrintWriter}

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.struct.Counter
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import edu.cmu.dynet._
import edu.cmu.dynet.Expression._
import RNN._
import org.clulab.utils.MathUtils

import scala.collection.mutable
import scala.util.Random

class RNN {
  var model:RNNParameters = _

  def train(trainSentences:Array[Array[Row]], devSentences:Array[Array[Row]], embeddingsFile:String): Unit = {
    val (w2i, t2i, c2i) = mkVocabs(trainSentences)
    logger.debug(s"Tag vocabulary has ${t2i.size} entries.")
    logger.debug(s"Word vocabulary has ${w2i.size} entries (including 1 for unknown).")
    logger.debug(s"Character vocabulary has ${c2i.size} entries.")

    initialize(w2i, t2i, c2i, embeddingsFile)
    update(trainSentences:Array[Array[Row]], devSentences:Array[Array[Row]])

    // TODO: save the model to disk here
  }

  def update(trainSentences: Array[Array[Row]], devSentences:Array[Array[Row]]): Unit = {
    //val trainer = new SimpleSGDTrainer(model.parameters, learningRate = 0.01f)
    val trainer = new RMSPropTrainer(model.parameters)
    var cummulativeLoss = 0.0
    var numTagged = 0
    var sentCount = 0
    var sentences = trainSentences
    val rand = new Random(RANDOM_SEED)
    for(epoch <- 0 until EPOCHS) {
      sentences = MathUtils.randomize(sentences, rand)

      logger.info(s"Started epoch $epoch.")
      for (index <- 0 to 1000) {
        val sentence = sentences(index)
//      }
//      for(sentence <- sentences) {
        sentCount += 1
        //logger.debug(s"Predicting sentence $sentCount: " + sentence.map(_.get(0)).mkString(", "))

        // predict probabilities for one sentence
        val words = sentence.map(_.get(0))
        val probs = predictSentence(words,  doDropout = false)
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

//      evaluate(devSentences, epoch + 1)
    }
  }

  def printCoNLLOutput(pw:PrintWriter, words:Array[String], golds:Array[String], preds:Array[String]): Unit = {
    for(i <- words.indices) {
      pw.println(words(i) + " " + golds(i) + " " + preds(i))
    }
    pw.println()
  }

  def accuracy(golds:Array[String], preds:Array[String]): (Int, Int) = {
    assert(golds.length == preds.length)
    var correct = 0
    for(e <- preds.zip(golds)) {
      if(e._1 == e._2) {
        correct += 1
      }
    }
    (golds.length, correct)
  }

  def evaluate(devSentences:Array[Array[Row]], epoch:Int): Unit = {
    var total = 0
    var correct = 0
    var correctMax = 0

    var totalNonO = 0
    var invalidNonO = 0

    val pw = new PrintWriter(new FileWriter("dev.output." + epoch))
    val pwm = new PrintWriter(new FileWriter("devmax.output." + epoch))
    logger.debug("Started evaluation on dev...")
    for(sent <- devSentences) {
      val words = sent.map(_.get(0))
      val golds = sent.map(_.get(1))

      val preds = predict(words)
      val (t, c) = accuracy(golds, preds)
      total += t
      correct += c

      printCoNLLOutput(pw, words, golds, preds)

      // analytics:
      //   how many labels break the BIO format?
      //   what is the max accuracy, if we correctly fixed all of them?
      val maxPreds = new Array[String](preds.length)
      for(i <- preds.indices) maxPreds(i) = preds(i)
      for(i <- preds.indices) {
        val crt = preds(i)
        val prev = if(i > 0) preds(i - 1) else "O"

        if(crt != "O") totalNonO += 1
        if(crt.startsWith("I-")) {
          if(! prev.startsWith("O") && crt.substring(2) == prev.substring(2)) {
            // valid output
          } else {
            invalidNonO += 1

            var j = i
            while(j >= 0 && maxPreds(j) != "O") {
              maxPreds(j) = golds(j)
              j -= 1
            }
          }
        }
      }

      val (_, cm) = accuracy(golds, maxPreds)
      correctMax += cm

      printCoNLLOutput(pwm, words, golds, maxPreds)
    }

    pw.close()
    pwm.close()
    logger.info(s"Accuracy on ${devSentences.length} dev sentences: " + correct.toDouble / total)
    logger.info("Percentage of invalid non-O labels: " + invalidNonO.toDouble / totalNonO + " (" + invalidNonO + "/" + totalNonO + ")")
    logger.info(s"Max accuracy on ${devSentences.length} dev sentences: " + correctMax.toDouble / total)
  }

  def sentenceLoss(tags:Iterable[String], probs:Iterable[Expression]): Expression = {
    val losses = new ExpressionVector()
    for(e <- tags.zip(probs)) {
      val t = e._1
      val prob = e._2
      val tid = model.t2i(t)
      val loss = pickNegLogSoftmax(prob, tid) // TODO: bad loss, fix me
      losses.add(loss)
    }
    Expression.sum(losses)
  }

  /**
    * Generates tag probabilities for the words in this sequence
    * @param words One training or testing sentence
    */
  def predictSentence(words: Array[String], doDropout:Boolean): Iterable[Expression] = {
    ComputationGraph.renew()

    val embeddings = words.map(mkEmbedding)

    val fwStates = transduce(embeddings, model.fwRnnBuilder)
    val bwStates = transduce(embeddings.reverse, model.bwRnnBuilder).toArray.reverse
    assert(fwStates.size == bwStates.length)
    val states = concatenateStates(fwStates, bwStates)

    val H = parameter(model.H)
    val O = parameter(model.O)

    states.map(s => if(doDropout) Expression.dropout(O * Expression.tanh(H * s), DROPOUT_PROB) else O * Expression.tanh(H * s))
  }

  def predict(words:Array[String]):Array[String] = synchronized {
    val scores = predictSentence(words, doDropout = false)
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
    val sanitized = word // word.toLowerCase() // Word2Vec.sanitizeWord(word)

    val wordEmbedding =
      if(model.w2i.contains(sanitized))
      // found the word in the known vocabulary
        lookup(model.lookupParameters, model.w2i(sanitized))
      else {
        // not found; return the embedding at position 0, which is reserved for unknown words
        lookup(model.lookupParameters, 0)
        //val caseIdx = casing(word)
        //lookup(model.lookupParameters, caseIdx)
      }

    // biLSTM over character embeddings
    val charEmbedding =
      mkCharacterEmbedding(word)

    // explicit features that capture the shape of the word
    /*
    val c = casing(word)
    val cases = new Array[Float](CASE_o + 1)
    for(i <- cases.indices) cases(i) = 0
    cases(c) = 1

    concatenate(wordEmbedding, charEmbedding, input(Dim(CASE_o + 1), new FloatVector(cases)))
    */
    concatenate(wordEmbedding, charEmbedding)
  }

  def mkCharacterEmbedding(word: String): Expression = {
    val charEmbeddings = new ArrayBuffer[Expression]()
    for(i <- word.indices) {
      if(model.c2i.contains(word.charAt(i)))
        charEmbeddings += lookup(model.charLookupParameters, model.c2i(word.charAt(i)))
    }
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
    model = mkParams(w2i, t2i, c2i)
    model.initialize(embeddingsFile) // Only necessary if constructing from scratch by training
    logger.debug("Completed initialization.")
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
) {
  protected def toFloatArray(doubles: Array[Double]): Array[Float] = {
    val floats = new Array[Float](doubles.length)
    for (i <- doubles.indices) {
      floats(i) = doubles(i).toFloat
    }
    floats
  }


  protected def add(dst:Array[Double], src:Array[Double]): Unit = {
    assert(dst.length == src.length)
    for(i <- dst.indices) {
      dst(i) += src(i)
    }
  }

  def initialize(embeddingsFile: String): Unit = {
    logger.debug(s"Loading embeddings from file $embeddingsFile...")
    val w2v = new Word2Vec(embeddingsFile) // Some(w2i.keySet))
    val unknownEmbed = new Array[Double](EMBEDDING_SIZE)
    for(i <- unknownEmbed.indices) unknownEmbed(i) = 0.0

    var unknownCount = 0
    for(word <- w2v.matrix.keySet){// w2i.keySet) {
      if(w2i.contains(word)) {
        lookupParameters.initialize(w2i(word), new FloatVector(toFloatArray(w2v.matrix(word))))
      } else {
        add(unknownEmbed, w2v.matrix(word))
        unknownCount += 1
      }
    }
    for(i <- unknownEmbed.indices) {
      unknownEmbed(i) /= unknownCount
    }
    lookupParameters.initialize(0, new FloatVector(toFloatArray(unknownEmbed)))

    /*
    val unknownEmbed = new Array[Array[Double]](TOTAL_CASES)
    for(i <- unknownEmbed.indices) {
      unknownEmbed(i) = new Array[Double](EMBEDDING_SIZE)
      for(j <- unknownEmbed(i).indices) {
        unknownEmbed(i)(j) = 0.0
      }
    }
    val unknownCounts = new Array[Int](TOTAL_CASES)
    for(i <- unknownCounts.indices) unknownCounts(i) = 0
    var totalCount = 0
    for(word <- w2v.matrix.keySet){// w2i.keySet) {
      if(w2i.contains(word)) {
        lookupParameters.initialize(w2i(word), new FloatVector(toFloatArray(w2v.matrix(word))))
      } else {
        val caseIdx = casing(word)
        add(unknownEmbed(caseIdx), w2v.matrix(word))
        unknownCounts(caseIdx) += 1
      }
      totalCount += 1
    }
    //println("before divide: " + unknownEmbed.mkString(", "))
    //println("unknownCount: " + unknownCount)
    //println("totalCount: " + totalCount)
    for(i <- 0 until TOTAL_CASES) {
      for (j <- unknownEmbed(i).indices) unknownEmbed(i)(j) /= unknownCounts(i).toDouble
      //println("after divide: " + unknownEmbed.mkString(", "))
      lookupParameters.initialize(i, new FloatVector(toFloatArray(unknownEmbed(i))))
    }
    */
    logger.debug(s"Loaded ${w2v.matrix.size} embeddings.")

  }
}

object RNN {
  val logger:Logger = LoggerFactory.getLogger(classOf[RNN])

  val EPOCHS = 1 // 2
  val RANDOM_SEED = 2522620396l // used for both DyNet, and the JVM seed for shuffling data
  val DROPOUT_PROB = 0.1f
  val EMBEDDING_SIZE = 300
  val RNN_STATE_SIZE = 50
  val NONLINEAR_SIZE = 32
  val RNN_LAYERS = 1
  val CHAR_RNN_LAYERS = 1
  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16

  // case features
  val CASE_x = 0
  val CASE_X = 1
  val CASE_Xx = 2
  val CASE_xX = 3
  val CASE_n = 4
  val CASE_o = 5
  val TOTAL_CASES:Int = CASE_o + 1

  def casing(w:String): Int = {
    if(w.charAt(0).isLetter) { // probably an actual word
      // count upper and lower-case chars
      var uppers = 0
      for(j <- 0 until w.length) {
        if(Character.isUpperCase(w.charAt(j))) {
          uppers += 1
        }
      }

      var v = CASE_x
      if (uppers == w.length) v = CASE_X
      else if (uppers == 1 && Character.isUpperCase(w.charAt(0))) v = CASE_Xx
      else if (uppers >= 1 && !Character.isUpperCase(w.charAt(0))) v = CASE_xX
      v
    } else if(isNumber(w))
      CASE_n
    else
      CASE_o
  }

  def isNumber(w:String): Boolean = {
    for(i <- 0 until w.length) {
      val c = w.charAt(i)
      if(! c.isDigit && c != '-' && c != '.' && c != ',')
        return false
    }
    true
  }

  def save(filename:String, rnnParameters: RNNParameters):Unit = {
    val modelSaver = new ModelSaver(filename)
    modelSaver.addModel(rnnParameters.parameters, "/all")
    modelSaver.done()
  }

  def load(filename:String, trainSentences:Array[Array[Row]], oldRnnParameters: RNNParameters):RNNParameters = {
    val (w2i, t2i, c2i) = mkVocabs(trainSentences)
    val (w2i2, t2i2, c2i2) = (oldRnnParameters.w2i, oldRnnParameters.t2i, oldRnnParameters.c2i)
    val model = mkParams(w2i, t2i, c2i) // This will not be initialized, but rather loaded from the file.

    new ModelLoader(filename).populateModel(model.parameters, "/all")
    model
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
        words += word // Word2Vec.sanitizeWord(word)
        for(i <- word.indices) {
          chars += word.charAt(i)
        }
        tags += token.get(1)
      }
    }

    val commonWords = new ListBuffer[String]
    /*
    for(i <- 0 until TOTAL_CASES)
      commonWords += "*unknown*" + i.toString // first position reserved for the unknown token
    */
    commonWords += "*unknown*"
    for(w <- words.keySet) {
      if(words.getCount(w) > 1) {
        commonWords += w
      }
    }

    val w2i = commonWords.sorted.zipWithIndex.toMap // These must be sorted for consistency across runs
    val t2i = tags.keySet.toList.sorted.zipWithIndex.toMap
    val c2i = chars.toList.sorted.zipWithIndex.toMap

    (w2i, t2i, c2i)
  }

  def mkParams(w2i:Map[String, Int], t2i:Map[String, Int], c2i:Map[Character, Int]): RNNParameters = {
    val parameters = new ParameterCollection()
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(EMBEDDING_SIZE))
    val embeddingSize = EMBEDDING_SIZE + 2 * CHAR_RNN_STATE_SIZE // + CASE_o + 1
    val fwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)
    val bwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)
    val H = parameters.addParameters(Dim(NONLINEAR_SIZE, 2 * RNN_STATE_SIZE))
    val O = parameters.addParameters(Dim(t2i.size, NONLINEAR_SIZE))
    val i2t = fromIndexToString(t2i)
    logger.debug("Created parameters.")

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)

    new RNNParameters(w2i, t2i, i2t, c2i, parameters, lookupParameters, fwBuilder, bwBuilder, H, O,
      charLookupParameters, charFwBuilder, charBwBuilder)
  }

  def main(args: Array[String]): Unit = {
    val trainFile = args(0)
    val devFile = args(1)
    val trainSentences = ColumnReader.readColumns(trainFile)
    val devSentences = ColumnReader.readColumns(devFile)
    val embeddingsFile = args(2)

    val rnn = new RNN()
    rnn.train(trainSentences, devSentences, embeddingsFile)

    val filename = "rnn.dat"
    save(filename, rnn.model)

    val pretrainedRnn = new RNN()
    val rnnParameters = load(filename, trainSentences, rnn.model)
    pretrainedRnn.model = rnnParameters

    save(filename + "2", pretrainedRnn.model)

    rnn.evaluate(devSentences, -1)
    pretrainedRnn.evaluate(devSentences, -1)
  }
}