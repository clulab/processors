package org.clulab.dynet

import java.io.PrintWriter

import edu.cmu.dynet.Expression.concatenate
import edu.cmu.dynet.{Dim, Expression, ExpressionVector, LookupParameter, LstmBuilder, ParameterCollection, RnnBuilder}
import org.clulab.lm.RnnLM.RANDOM
import org.clulab.struct.Counter
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.dynet.Utils._
import org.clulab.utils.{Configured, Serializer}

import scala.util.Random

/**
 * This layer takes a sequence of words and produces a sequence of Expression that stores the words' full embeddings
 * @author Mihai
 */
class EmbeddingLayer (val parameters:ParameterCollection,
                      val w2i:Map[String, Int], // word to index
                      val w2f:Counter[String], // word to frequency
                      val c2i:Map[Char, Int], // character to index
                      val tag2i:Option[Map[String, Int]], // POS tag to index
                      val learnedWordEmbeddingSize: Int, // size of the learned word embedding
                      val charEmbeddingSize: Int, // size of the character embedding
                      val charRnnStateSize: Int, // size of each one of the char-level RNNs
                      val posTagEmbeddingSize: Int, // size of the POS tag embedding
                      val positionEmbeddingSize: Int,
                      val positionWindowSize: Int, // window considered for position values (relative to predicate)
                      val wordLookupParameters:LookupParameter,
                      val charLookupParameters:LookupParameter,
                      val charFwRnnBuilder:RnnBuilder, // RNNs for the character representation
                      val charBwRnnBuilder:RnnBuilder,
                      val posTagLookupParameters:Option[LookupParameter],
                      val positionLookupParameters:Option[LookupParameter],
                      val dropoutProb: Float = EmbeddingLayer.DROPOUT_PROB) extends InitialLayer {

  lazy val constEmbedder: ConstEmbeddings = ConstEmbeddingsGlove()

  override def needsPosTags: Boolean = posTagEmbeddingSize > 0

  override def forward(words: IndexedSeq[String],
                       tags: Option[IndexedSeq[String]],
                       predicatePosition: Option[Int],
                       doDropout: Boolean): ExpressionVector = {
    setCharRnnDropout(doDropout)

    // const word embeddings such as GloVe
    val constEmbeddings = constEmbedder.mkEmbeddings(words)
    assert(constEmbeddings.length == words.length)
    if(tags.isDefined) assert(tags.get.length == words.length)

    // build the word embeddings one by one
    val embeddings = new ExpressionVector()
    for(i <- words.indices) {
      val tag = if(tags.isDefined) Some(tags.get(i)) else None
      embeddings.add(mkEmbedding(words(i), i, tag, predicatePosition, constEmbeddings(i), doDropout))
    }

    embeddings
  }

  private def mkEmbedding(word: String,
                          wordPosition: Int,
                          tag: Option[String],
                          predicatePosition: Option[Int],
                          constEmbedding: Expression,
                          doDropout: Boolean): Expression = {
    //
    // Learned word embeddings
    // These are initialized randomly, and updated during backprop
    //
    var id = w2i.getOrElse(word, 0) // 0 reserved for UNK in the vocab
    // sample uniformly with prob 0.5 from singletons; move all other singletons to UNK
    if(doDropout && id > 0 && w2f.getCount(word) == 1 && RANDOM.nextDouble() < 0.5) id = 0
    val learnedWordEmbedding = Expression.lookup(wordLookupParameters, id)

    //
    // biLSTM over character embeddings
    //
    val charEmbedding =
      Utils.mkCharacterEmbedding(word, c2i, charLookupParameters, charFwRnnBuilder, charBwRnnBuilder)

    //
    // POS tag embedding
    //
    val posTagEmbed =
      if(tag.nonEmpty) {
        assert(posTagLookupParameters.nonEmpty)
        assert(tag2i.nonEmpty)
        Some(Expression.lookup(posTagLookupParameters.get, tag2i.get.getOrElse(tag.get, 0)))
      } else {
        None
      }

    //
    // 1 if this word is the predicate
    //
    val predEmbed =
    if(predicatePosition.nonEmpty) {
      Some(Expression.input(if(wordPosition == predicatePosition.get) 1f else 0f))
    } else {
      None
    }

    //
    // Position embedding, relative to the position of the predicate
    // We cut the distance down to values inside the window [-positionWindowSize, +positionWindowSize]
    //
    val positionEmbedding =
      if(predicatePosition.nonEmpty) {
        assert(positionLookupParameters.nonEmpty)
        var dist = wordPosition - predicatePosition.get
        if (dist < - positionWindowSize) dist = - positionWindowSize - 1
        if(dist > positionWindowSize) dist = positionWindowSize + 1
        val posIndex = dist + positionWindowSize + 1
        Some(Expression.lookup(positionLookupParameters.get, posIndex))
      } else {
        None
      }

    // The final word embedding is a concatenation of all these
    val embed =
      if(posTagEmbed.nonEmpty) {
        if(positionEmbedding.nonEmpty) {
          assert(predEmbed.nonEmpty)
          concatenate(constEmbedding,
            learnedWordEmbedding,
            charEmbedding,
            posTagEmbed.get,
            predEmbed.get,
            positionEmbedding.get)
        } else {
          concatenate(constEmbedding,
            learnedWordEmbedding,
            charEmbedding,
            posTagEmbed.get)
        }
      } else {
        concatenate(constEmbedding,
          learnedWordEmbedding,
          charEmbedding)
      }

    assert(embed.dim().get(0) == outDim)
    embed
  }

  override def outDim: Int = {
    val posTagDim = if(posTagLookupParameters.nonEmpty) posTagEmbeddingSize else 0
    val positionDim = if(positionLookupParameters.nonEmpty) positionEmbeddingSize else 0
    val predicateDim = if(positionLookupParameters.nonEmpty) 1 else 0

    constEmbedder.dim +
    learnedWordEmbeddingSize +
    charRnnStateSize * 2 +
    posTagDim +
    positionDim +
    predicateDim
  }

  private def setCharRnnDropout(doDropout: Boolean): Unit = {
    setRnnDropout(charFwRnnBuilder, dropoutProb, doDropout)
    setRnnDropout(charBwRnnBuilder, dropoutProb, doDropout)
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    save(printWriter, w2i, "w2i")
    save(printWriter, w2f, "w2f")
    save(printWriter, c2i, "c2i")
    if(tag2i.nonEmpty) {
      save(printWriter, 1, "hasTag2i")
      save(printWriter, tag2i.get, "tag2i")
    } else {
      save(printWriter, 0, "hasTag2i")
    }
    save(printWriter, learnedWordEmbeddingSize, "learnedWordEmbeddingSize")
    save(printWriter, charEmbeddingSize, "charEmbeddingSize")
    save(printWriter, charRnnStateSize, "charRnnStateSize")
    save(printWriter, posTagEmbeddingSize, "posTagEmbeddingSize")
    save(printWriter, positionEmbeddingSize, "positionEmbeddingSize")
    save(printWriter, positionWindowSize, "positionWindowSize")
  }

  override def toString: String = {
    s"EmbeddingLayer($outDim)"
  }
}

object EmbeddingLayer {
  val logger:Logger = LoggerFactory.getLogger(classOf[EmbeddingLayer])

  val RANDOM = new Random(Utils.RANDOM_SEED)

  val DROPOUT_PROB = 0.2f

  def load(parameters: ParameterCollection,
           x2iIterator:Iterator[String]): EmbeddingLayer = {
    //
    // load the x2i info
    //
    val byLineCharMapBuilder = new ByLineCharIntMapBuilder()
    val byLineCounterBuilder = new ByLineStringCounterBuilder()
    val byLineStringMapBuilder = new ByLineStringMapBuilder()
    val byLineIntBuilder = new ByLineIntBuilder()

    val w2i = byLineStringMapBuilder.build(x2iIterator)
    val w2f = byLineCounterBuilder.build(x2iIterator)
    val c2i = byLineCharMapBuilder.build(x2iIterator)
    val hasTag2i = byLineIntBuilder.build(x2iIterator)
    val tag2i =
      if(hasTag2i == 1) {
        Some(byLineStringMapBuilder.build(x2iIterator))
      } else {
        None
      }
    val learnedWordEmbeddingSize = byLineIntBuilder.build(x2iIterator)
    val charEmbeddingSize = byLineIntBuilder.build(x2iIterator)
    val charRnnStateSize = byLineIntBuilder.build(x2iIterator)
    val posTagEmbeddingSize = byLineIntBuilder.build(x2iIterator)
    val positionEmbeddingSize = byLineIntBuilder.build(x2iIterator)
    val positionWindowSize = byLineIntBuilder.build(x2iIterator)

    //
    // make the loadable parameters
    //
    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(learnedWordEmbeddingSize))
    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(charEmbeddingSize))
    val charFwRnnBuilder = new LstmBuilder(1, charEmbeddingSize, charRnnStateSize, parameters)
    val charBwRnnBuilder = new LstmBuilder(1, charEmbeddingSize, charRnnStateSize, parameters)

    val posTagLookupParameters =
      if(hasTag2i == 1) {
        assert(tag2i.nonEmpty)
        Some(parameters.addLookupParameters(tag2i.get.size, Dim(posTagEmbeddingSize)))
      } else {
        None
      }

    val positionLookupParameters =
      if(positionEmbeddingSize > 0) {
        // the +3 is needed for: position 0, 1 to the left of the window, and 1 to the right of the window
        Some(parameters.addLookupParameters(positionWindowSize * 2 + 3, Dim(positionEmbeddingSize)))
      } else {
        None
      }

    new EmbeddingLayer(
      parameters,
      w2i, w2f, c2i, tag2i,
      learnedWordEmbeddingSize,
      charEmbeddingSize,
      charRnnStateSize,
      posTagEmbeddingSize,
      positionEmbeddingSize,
      positionWindowSize,
      wordLookupParameters,
      charLookupParameters,
      charFwRnnBuilder,
      charBwRnnBuilder,
      posTagLookupParameters,
      positionLookupParameters)

  }

  def initialize(config: Configured,
                 paramPrefix: String,
                 parameters: ParameterCollection,
                 wordCounter: Counter[String]): Option[InitialLayer] = {
    if(! config.contains(paramPrefix)) {
      return None
    }

    val learnedWordEmbeddingSize = config.getArgInt(paramPrefix + ".learnedWordEmbeddingSize", Some(128))
    val charEmbeddingSize = config.getArgInt(paramPrefix + ".charEmbeddingSize", Some(32))
    val charRnnStateSize = config.getArgInt(paramPrefix + ".charRnnStateSize", Some(16))
    val posTagEmbeddingSize = config.getArgInt(paramPrefix + ".posTagEmbeddingSize", Some(-1))
    val positionEmbeddingSize = config.getArgInt(paramPrefix + ".positionEmbeddingSize", Some(-1))
    val positionWindowSize = config.getArgInt(paramPrefix + ".positionWindowSize", Some(-1))
    val dropoutProb = config.getArgFloat(paramPrefix + ".dropoutProb", Some(EmbeddingLayer.DROPOUT_PROB))

    // the word at position 0 is always reserved for UNK
    val wordList = List(Utils.UNK_WORD) ++ wordCounter.keySet.toList.sorted
    val w2i = wordList.zipWithIndex.toMap

    val wordLookupParameters:LookupParameter = parameters.addLookupParameters(w2i.size, Dim(learnedWordEmbeddingSize))

    val c2iFilename = config.getArgString(paramPrefix + ".c2i", Some("org/clulab/c2i-en.txt"))
    val c2i = Serializer.using(Utils.newSource(c2iFilename)) { source =>
      val byLineCharMapBuilder = new Utils.ByLineCharIntMapBuilder()
      val lines = source.getLines()
      val c2i = byLineCharMapBuilder.build(lines)
      c2i
    }

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(charEmbeddingSize))
    val charFwRnnBuilder = new LstmBuilder(1, charEmbeddingSize, charRnnStateSize, parameters)
    val charBwRnnBuilder = new LstmBuilder(1, charEmbeddingSize, charRnnStateSize, parameters)

    val (tag2i, posTagLookupParameters) =
      if(posTagEmbeddingSize > 0) {
        val tag2i = Utils.readString2Ids(config.getArgString(paramPrefix + ".tag2i", Some("org/clulab/lm/tag2i-en.txt")))
        val posTagLookupParameters = parameters.addLookupParameters(tag2i.size, Dim(posTagEmbeddingSize))
        (Some(tag2i), Some(posTagLookupParameters))
      } else {
        (None, None)
      }

    val positionLookupParameters =
      if(positionEmbeddingSize > 0) {
        // Position embeddings [-positionWindowSize, positionWindowSize] + < positionWindowSize + > positionWindowSize. total = 43
        val positionLookupParameters = parameters.addLookupParameters(positionWindowSize * 2 + 3, Dim(positionEmbeddingSize))
        Some(positionLookupParameters)
      } else {
        None
      }

    val layer = new EmbeddingLayer(
      parameters:ParameterCollection,
      w2i, wordCounter, c2i, tag2i,
      learnedWordEmbeddingSize,
      charEmbeddingSize,
      charRnnStateSize,
      posTagEmbeddingSize,
      positionEmbeddingSize,
      positionWindowSize,
      wordLookupParameters,
      charLookupParameters,
      charFwRnnBuilder,
      charBwRnnBuilder,
      posTagLookupParameters,
      positionLookupParameters,
      dropoutProb)

    Some(layer)
  }

}
