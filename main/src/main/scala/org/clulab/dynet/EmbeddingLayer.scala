package org.clulab.dynet

import java.io.PrintWriter

import edu.cmu.dynet.Expression.concatenate
import edu.cmu.dynet.{Dim, Expression, ExpressionVector, LookupParameter, LstmBuilder, ParameterCollection, RnnBuilder}
import org.clulab.struct.Counter
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.dynet.Utils._
import org.clulab.utils.{Configured, Serializer}

import EmbeddingLayer._

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
                      val ne2i:Option[Map[String, Int]], // NE tag to index
                      val learnedWordEmbeddingSize: Int, // size of the learned word embedding
                      val charEmbeddingSize: Int, // size of the character embedding
                      val charRnnStateSize: Int, // size of each one of the char-level RNNs
                      val posTagEmbeddingSize: Int, // size of the POS tag embedding
                      val neTagEmbeddingSize: Int, // size of the NE tag embedding
                      val distanceEmbeddingSize: Int,
                      val distanceWindowSize: Int, // window considered for distance values (relative to predicate)
                      val positionEmbeddingSize: Int,
                      val useIsPredicate: Boolean, // if true, add a Boolean bit to indicate if current word is the predicate
                      val wordLookupParameters:LookupParameter,
                      val charLookupParameters:LookupParameter,
                      val charFwRnnBuilder:RnnBuilder, // RNNs for the character representation
                      val charBwRnnBuilder:RnnBuilder,
                      val posTagLookupParameters:Option[LookupParameter],
                      val neTagLookupParameters:Option[LookupParameter],
                      val distanceLookupParameters:Option[LookupParameter],
                      val positionLookupParameters:Option[LookupParameter],
                      val dropoutProb: Float) extends InitialLayer {

  override def forward(sentence: AnnotatedSentence,
                       constEmbeddings: ConstEmbeddingParameters,
                       doDropout: Boolean): ExpressionVector = {
    setCharRnnDropout(doDropout)

    val words = sentence.words
    val tags = sentence.posTags
    val nes = sentence.neTags
    val headPositions = sentence.headPositions

    // const word embeddings such as GloVe
    val constEmbeddingsExpressions = mkConstEmbeddings(words, constEmbeddings)
    assert(constEmbeddingsExpressions.length == words.length)
    if(tags.isDefined) assert(tags.get.length == words.length)
    if(nes.isDefined) assert(nes.get.length == words.length)
    if(headPositions.isDefined) assert(headPositions.get.length == words.length)

    // build the word embeddings one by one
    val embeddings = new ExpressionVector()
    for(i <- words.indices) {
      val tag = if(tags.isDefined) Some(tags.get(i)) else None
      val ne = if(nes.isDefined) Some(nes.get(i)) else None
      val headPosition = if(headPositions.isDefined) Some(headPositions.get(i)) else None
      embeddings.add(mkEmbedding(words(i), i, tag, ne, headPosition, constEmbeddingsExpressions(i), doDropout))
    }

    embeddings
  }

  private def mkConstEmbeddings(words: IndexedSeq[String],
                                constEmbeddings: ConstEmbeddingParameters): ExpressionVector = {
    val embeddings = new ExpressionVector()
    // the position in the sentence serves as index in the constLookupParams
    for(word <- words) {
      val idx = constEmbeddings.w2i.getOrElse(word, 0) // 0 is reserved for the unknown embedding
      embeddings.add(Expression.constLookup(constEmbeddings.lookupParameters, idx))
    }
    embeddings
  }

  private def mkEmbedding(word: String,
                          wordPosition: Int,
                          tag: Option[String],
                          ne: Option[String],
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
      if(tag.nonEmpty && posTagLookupParameters.nonEmpty) {
        assert(tag2i.nonEmpty)
        Some(Expression.lookup(posTagLookupParameters.get, tag2i.get.getOrElse(tag.get, 0)))
      } else {
        None
      }

    //
    // NE tag embedding
    //
    val neTagEmbed =
      if(ne.nonEmpty && neTagLookupParameters.nonEmpty) {
        assert(ne2i.nonEmpty)
        Some(Expression.lookup(neTagLookupParameters.get, ne2i.get.getOrElse(ne.get, 0)))
      } else {
        None
      }

    //
    // 1 if this word is the predicate
    //
    val predEmbed =
    if(predicatePosition.nonEmpty && useIsPredicate) {
      Some(Expression.input(if(wordPosition == predicatePosition.get) 1f else 0f))
    } else {
      None
    }

    //
    // Distance embedding, relative to the distance to the predicate
    // We cut the distance down to values inside the window [-distanceWindowSize, +distanceWindowSize]
    //
    val distanceEmbedding =
      if(predicatePosition.nonEmpty && distanceLookupParameters.nonEmpty) {
        var dist = wordPosition - predicatePosition.get
        if (dist < - distanceWindowSize) dist = - distanceWindowSize - 1
        if(dist > distanceWindowSize) dist = distanceWindowSize + 1
        val posIndex = dist + distanceWindowSize + 1
        Some(Expression.lookup(distanceLookupParameters.get, posIndex))
      } else {
        None
      }

    //
    // Embedding that captures the absolute position of the token in the sentence
    //
    val positionEmbedding =
      if(positionLookupParameters.nonEmpty) {
        val value = if(wordPosition >= 100) 100 else wordPosition
        Some(Expression.lookup(positionLookupParameters.get, value))
      } else {
        None
      }

    // The final word embedding is a concatenation of all these
    val embedParts = new ExpressionVector()
    embedParts.add(constEmbedding)
    embedParts.add(learnedWordEmbedding)
    embedParts.add(charEmbedding)
    if(posTagEmbed.nonEmpty) embedParts.add(posTagEmbed.get)
    if(neTagEmbed.nonEmpty) embedParts.add(neTagEmbed.get)
    if(distanceEmbedding.nonEmpty) embedParts.add(distanceEmbedding.get)
    if(positionEmbedding.nonEmpty) embedParts.add(positionEmbedding.get)
    if(predEmbed.nonEmpty) embedParts.add(predEmbed.get)

    val embed = concatenate(embedParts)
    assert(embed.dim().get(0) == outDim)
    embed
  }

  override def outDim: Int = {
    val posTagDim = if(posTagLookupParameters.nonEmpty) posTagEmbeddingSize else 0
    val neTagDim = if(neTagLookupParameters.nonEmpty) neTagEmbeddingSize else 0
    val distanceDim = if(distanceLookupParameters.nonEmpty) distanceEmbeddingSize else 0
    val predicateDim = if(distanceLookupParameters.nonEmpty && useIsPredicate) 1 else 0
    val positionDim = if(positionLookupParameters.nonEmpty) positionEmbeddingSize else 0

    ConstEmbeddingsGlove.dim +
    learnedWordEmbeddingSize +
    charRnnStateSize * 2 +
    posTagDim +
    neTagDim +
    distanceDim +
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
    saveCharMap(printWriter, c2i, "c2i")
    if(tag2i.nonEmpty) {
      save(printWriter, 1, "hasTag2i")
      save(printWriter, tag2i.get, "tag2i")
    } else {
      save(printWriter, 0, "hasTag2i")
    }
    if(ne2i.nonEmpty) {
      save(printWriter, 1, "hasNe2i")
      save(printWriter, ne2i.get, "ne2i")
    } else {
      save(printWriter, 0, "hasNe2i")
    }
    save(printWriter, learnedWordEmbeddingSize, "learnedWordEmbeddingSize")
    save(printWriter, charEmbeddingSize, "charEmbeddingSize")
    save(printWriter, charRnnStateSize, "charRnnStateSize")
    save(printWriter, posTagEmbeddingSize, "posTagEmbeddingSize")
    save(printWriter, neTagEmbeddingSize, "neTagEmbeddingSize")
    save(printWriter, distanceEmbeddingSize, "distanceEmbeddingSize")
    save(printWriter, distanceWindowSize, "distanceWindowSize")
    save(printWriter, if(useIsPredicate) 1 else 0, "useIsPredicate")
    save(printWriter, positionEmbeddingSize, "positionEmbeddingSize")
    save(printWriter, dropoutProb, "dropoutProb")
  }

  override def toString: String = {
    s"EmbeddingLayer($outDim)"
  }
}

object EmbeddingLayer {
  val logger:Logger = LoggerFactory.getLogger(classOf[EmbeddingLayer])

  val RANDOM = new Random(Utils.RANDOM_SEED)

  val DEFAULT_DROPOUT_PROB: Float = Utils.DEFAULT_DROPOUT_PROBABILITY
  val DEFAULT_LEARNED_WORD_EMBEDDING_SIZE: Int = 128
  val DEFAULT_CHAR_EMBEDDING_SIZE: Int = 32
  val DEFAULT_CHAR_RNN_STATE_SIZE: Int = 16
  val DEFAULT_POS_TAG_EMBEDDING_SIZE: Int = -1 // no POS tag embeddings by default
  val DEFAULT_NE_TAG_EMBEDDING_SIZE: Int = -1 // no NE tag embeddings by default
  val DEFAULT_DISTANCE_EMBEDDING_SIZE: Int = -1 // no distance embeddings by default
  val DEFAULT_POSITION_EMBEDDING_SIZE: Int = -1 // no position embeddings by default
  val DEFAULT_DISTANCE_WINDOW_SIZE: Int = -1
  val DEFAULT_USE_IS_PREDICATE: Int = -1

  def load(parameters: ParameterCollection,
           x2iIterator:BufferedIterator[String]): EmbeddingLayer = {
    //
    // load the x2i info
    //
    val byLineCharMapBuilder = new ByLineCharIntMapBuilder()
    val byLineCounterBuilder = new ByLineStringCounterBuilder()
    val byLineStringMapBuilder = new ByLineStringMapBuilder()
    val byLineIntBuilder = new ByLineIntBuilder()
    val byLineFloatBuilder = new ByLineFloatBuilder()

    val w2i = byLineStringMapBuilder.build(x2iIterator, "w2i")
    val w2f = byLineCounterBuilder.build(x2iIterator, "w2f")
    val c2i = byLineCharMapBuilder.build(x2iIterator, "c2i")
    val hasTag2i = byLineIntBuilder.build(x2iIterator, "hasTag2i", 0)
    val tag2i =
      if(hasTag2i == 1) {
        Some(byLineStringMapBuilder.build(x2iIterator))
      } else {
        None
      }
    val hasNe2i = byLineIntBuilder.build(x2iIterator, "hasNe2i", 0)
    val ne2i =
      if(hasNe2i == 1) {
        Some(byLineStringMapBuilder.build(x2iIterator))
      } else {
        None
      }

    val learnedWordEmbeddingSize =
      byLineIntBuilder.build(x2iIterator,"learnedWordEmbeddingSize", DEFAULT_LEARNED_WORD_EMBEDDING_SIZE)
    val charEmbeddingSize =
      byLineIntBuilder.build(x2iIterator,"charEmbeddingSize", DEFAULT_CHAR_EMBEDDING_SIZE)
    val charRnnStateSize =
      byLineIntBuilder.build(x2iIterator, "charRnnStateSize", DEFAULT_CHAR_RNN_STATE_SIZE)
    val posTagEmbeddingSize =
      byLineIntBuilder.build(x2iIterator, "posTagEmbeddingSize", DEFAULT_POS_TAG_EMBEDDING_SIZE)
    val neTagEmbeddingSize =
      byLineIntBuilder.build(x2iIterator, "neTagEmbeddingSize", DEFAULT_NE_TAG_EMBEDDING_SIZE)
    val distanceEmbeddingSize =
      byLineIntBuilder.build(x2iIterator, "distanceEmbeddingSize", DEFAULT_DISTANCE_EMBEDDING_SIZE)
    val distanceWindowSize =
      byLineIntBuilder.build(x2iIterator, "distanceWindowSize", DEFAULT_DISTANCE_WINDOW_SIZE)
    val useIsPredicateAsInt =
      byLineIntBuilder.build(x2iIterator, "useIsPredicate", DEFAULT_USE_IS_PREDICATE)
    val useIsPredicate = useIsPredicateAsInt == 1
    val positionEmbeddingSize =
      byLineIntBuilder.build(x2iIterator, "positionEmbeddingSize", DEFAULT_POSITION_EMBEDDING_SIZE)
    val dropoutProb =
      byLineFloatBuilder.build(x2iIterator, "dropoutProb", DEFAULT_DROPOUT_PROB)

    //
    // make the loadable parameters
    //
    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(learnedWordEmbeddingSize))
    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(charEmbeddingSize))
    // The following line would normally provoke construction of the initial ComputationGraph
    // and do that outside of a synchronized area.  This is avoided by ensuring that construction
    // happens in Utils.initializeDyNet instead, just to be safe.
    val charFwRnnBuilder = new LstmBuilder(1, charEmbeddingSize, charRnnStateSize, parameters)
    val charBwRnnBuilder = new LstmBuilder(1, charEmbeddingSize, charRnnStateSize, parameters)

    val posTagLookupParameters =
      if(hasTag2i == 1) {
        assert(tag2i.nonEmpty)
        Some(parameters.addLookupParameters(tag2i.get.size, Dim(posTagEmbeddingSize)))
      } else {
        None
      }

    val neTagLookupParameters =
      if(hasNe2i == 1) {
        assert(ne2i.nonEmpty)
        //println(s"LOAD: neTagLookupParameters has dim ${ne2i.get.size} x $neTagEmbeddingSize")
        Some(parameters.addLookupParameters(ne2i.get.size, Dim(neTagEmbeddingSize)))
      } else {
        None
      }

    val distanceLookupParameters =
      if(distanceEmbeddingSize > 0) {
        // the +3 is needed for: distance 0, 1 to the left of the window, and 1 to the right of the window
        Some(parameters.addLookupParameters(distanceWindowSize * 2 + 3, Dim(distanceEmbeddingSize)))
      } else {
        None
      }

    val positionLookupParameters =
      if(positionEmbeddingSize > 0) {
        Some(parameters.addLookupParameters(101, Dim(positionEmbeddingSize)))
      } else {
        None
      }

    new EmbeddingLayer(
      parameters,
      w2i, w2f, c2i, tag2i, ne2i,
      learnedWordEmbeddingSize,
      charEmbeddingSize,
      charRnnStateSize,
      posTagEmbeddingSize,
      neTagEmbeddingSize,
      distanceEmbeddingSize,
      distanceWindowSize,
      positionEmbeddingSize,
      useIsPredicate,
      wordLookupParameters,
      charLookupParameters,
      charFwRnnBuilder,
      charBwRnnBuilder,
      posTagLookupParameters,
      neTagLookupParameters,
      distanceLookupParameters,
      positionLookupParameters,
      dropoutProb)
  }

  def initialize(config: Configured,
                 paramPrefix: String,
                 parameters: ParameterCollection,
                 wordCounter: Counter[String]): Option[InitialLayer] = {
    if(! config.contains(paramPrefix)) {
      return None
    }

    val learnedWordEmbeddingSize =
      config.getArgInt(paramPrefix + ".learnedWordEmbeddingSize",
        Some(DEFAULT_LEARNED_WORD_EMBEDDING_SIZE))
    val charEmbeddingSize =
      config.getArgInt(paramPrefix + ".charEmbeddingSize",
        Some(DEFAULT_CHAR_EMBEDDING_SIZE))
    val charRnnStateSize =
      config.getArgInt(paramPrefix + ".charRnnStateSize",
        Some(DEFAULT_CHAR_RNN_STATE_SIZE))
    val posTagEmbeddingSize =
      config.getArgInt(paramPrefix + ".posTagEmbeddingSize",
        Some(DEFAULT_POS_TAG_EMBEDDING_SIZE))
    val neTagEmbeddingSize =
      config.getArgInt(paramPrefix + ".neTagEmbeddingSize",
        Some(DEFAULT_NE_TAG_EMBEDDING_SIZE))
    val distanceEmbeddingSize =
      config.getArgInt(paramPrefix + ".distanceEmbeddingSize",
        Some(DEFAULT_DISTANCE_EMBEDDING_SIZE))
    val distanceWindowSize =
      config.getArgInt(paramPrefix + ".distanceWindowSize",
        Some(DEFAULT_DISTANCE_WINDOW_SIZE))
    val useIsPredicate =
      config.getArgBoolean(paramPrefix + ".useIsPredicate",
        Some(DEFAULT_USE_IS_PREDICATE == 1))
    val positionEmbeddingSize =
      config.getArgInt(paramPrefix + ".positionEmbeddingSize",
        Some(DEFAULT_POSITION_EMBEDDING_SIZE))
    val dropoutProb =
      config.getArgFloat(paramPrefix + ".dropoutProb",
        Some(EmbeddingLayer.DEFAULT_DROPOUT_PROB))

    // the word at position 0 is always reserved for UNK
    val wordList = List(Utils.UNK_WORD) ++ wordCounter.keySet.toList.sorted
    val w2i = wordList.zipWithIndex.toMap

    val wordLookupParameters:LookupParameter = parameters.addLookupParameters(w2i.size, Dim(learnedWordEmbeddingSize))

    val c2iFilename = config.getArgString(paramPrefix + ".c2i", Some("org/clulab/c2i-en.txt"))
    val c2i = Serializer.using(Utils.newSource(c2iFilename)) { source =>
      val byLineCharMapBuilder = new Utils.ByLineCharIntMapBuilder()
      val lines = source.getLines().buffered
      val c2i = byLineCharMapBuilder.build(lines)
      //println(s"c2i has size ${c2i.size}")
      c2i
    }

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(charEmbeddingSize))
    val charFwRnnBuilder = new LstmBuilder(1, charEmbeddingSize, charRnnStateSize, parameters)
    val charBwRnnBuilder = new LstmBuilder(1, charEmbeddingSize, charRnnStateSize, parameters)

    val (tag2i, posTagLookupParameters) =
      if(posTagEmbeddingSize > 0) {
        val tag2i = Utils.readString2Ids(config.getArgString(paramPrefix + ".tag2i", Some("org/clulab/tag2i-en.txt")))
        val posTagLookupParameters = parameters.addLookupParameters(tag2i.size, Dim(posTagEmbeddingSize))
        (Some(tag2i), Some(posTagLookupParameters))
      } else {
        (None, None)
      }

    val (ne2i, neTagLookupParameters) =
      if(neTagEmbeddingSize > 0) {
        val ne2i = Utils.readString2Ids(config.getArgString(paramPrefix + ".ne2i", Some("org/clulab/ne2i-en.txt")))
        val neTagLookupParameters = parameters.addLookupParameters(ne2i.size, Dim(neTagEmbeddingSize))
        //println(s"INITIALIZE: neTagLookupParameters has dim ${ne2i.size} x $neTagEmbeddingSize")
        (Some(ne2i), Some(neTagLookupParameters))
      } else {
        (None, None)
      }

    val distanceLookupParameters =
      if(distanceEmbeddingSize > 0) {
        // Position embeddings [-distanceWindowSize, distanceWindowSize] + < distanceWindowSize + > distanceWindowSize. total = 43
        val distanceLookupParameters = parameters.addLookupParameters(distanceWindowSize * 2 + 3, Dim(distanceEmbeddingSize))
        Some(distanceLookupParameters)
      } else {
        None
      }

    val positionLookupParameters =
      if(positionEmbeddingSize > 0) {
        val positionLookupParameters = parameters.addLookupParameters(101, Dim(positionEmbeddingSize))
        Some(positionLookupParameters)
      } else {
        None
      }

    val layer = new EmbeddingLayer(
      parameters:ParameterCollection,
      w2i, wordCounter, c2i, tag2i, ne2i,
      learnedWordEmbeddingSize,
      charEmbeddingSize,
      charRnnStateSize,
      posTagEmbeddingSize,
      neTagEmbeddingSize,
      distanceEmbeddingSize,
      distanceWindowSize,
      positionEmbeddingSize,
      useIsPredicate,
      wordLookupParameters,
      charLookupParameters,
      charFwRnnBuilder,
      charBwRnnBuilder,
      posTagLookupParameters,
      neTagLookupParameters,
      distanceLookupParameters,
      positionLookupParameters,
      dropoutProb)

    Some(layer)
  }

}
