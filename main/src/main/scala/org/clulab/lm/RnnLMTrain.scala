package org.clulab.lm

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{Dim, LstmBuilder, ParameterCollection}
import org.clulab.sequences.LstmUtils
import org.clulab.sequences.LstmUtils.{mkDynetFilename, mkX2iFilename}
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser

/**
 * Constructs the RnnLM model
 * This starts similarly to Lample et al. (2016), but it includes a LM training component as well
 * This contains static word embeddings (GloVe) + biLSTM character embeddings that are learned with the task,
 *   which then feed into a word-level biLSTM
 */
object RnnLMTrain {
  val logger:Logger = LoggerFactory.getLogger(classOf[RnnLMTrain])

  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16
  val RNN_STATE_SIZE = 256

  def main(args: Array[String]): Unit = {
    LstmUtils.initializeDyNet() // autoBatch = true, mem = "1660,1664,2496,1400")
    val configName = "rnnlm-en"
    val config = new FlairConfig(ConfigFactory.load(configName))

    //
    // Load the character map
    //
    logger.debug(s"Loading the character map...")
    val c2iFilename = config.getArgString("rnnlm.train.c2i", None)
    val c2i = Serializer.using(LstmUtils.newSource(c2iFilename)) { source =>
      val byLineCharMapBuilder = new LstmUtils.ByLineCharIntMapBuilder()
      val lines = source.getLines()
      val c2i = byLineCharMapBuilder.build(lines)
      c2i
    }
    logger.debug(s"Loaded a character map with ${c2i.keySet.size} entries.")

    //
    // Load the word embeddings
    //
    logger.debug("Loading word embeddings...")
    val embedFilename = config.getArgString("rnnlm.train.embed", None)
    val docFreqFilename = config.getArgString("rnnlm.train.docFreq", None)
    val minFreq = config.getArgInt("rnnlm.train.minWordFreq", Some(100))
    val w2v = LstmUtils.loadEmbeddings(Some(docFreqFilename), minFreq, embedFilename,
      Some(config.getArgString("rnnlm.train.mandatoryWords", None)))
    val w2i = LstmUtils.mkWordVocab(w2v)

    //
    // This stores all parameters in the RnnLM
    //
    val parameters = new ParameterCollection()

    //
    // Convert the word embeddings we loaded above into DyNet LookupParameters
    //
    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(w2v.dimensions))
    LstmUtils.initializeEmbeddings(w2v, w2i, wordLookupParameters)
    logger.debug("Completed loading word embeddings.")

    //
    // Character embeddings and character biLSTM, initialized randomly
    //
    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwRnnBuilder = new LstmBuilder(1, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwRnnBuilder = new LstmBuilder(1, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)

    val embeddingSize = 2 * CHAR_RNN_STATE_SIZE + w2v.dimensions
    val fwBuilder = new LstmBuilder(1, embeddingSize, RNN_STATE_SIZE, parameters)
    val bwBuilder = new LstmBuilder(1, embeddingSize, RNN_STATE_SIZE, parameters)

    //
    // Create the LM object
    //
    val lm = new RnnLM(w2i, c2i,
      RNN_STATE_SIZE, CHAR_RNN_STATE_SIZE,
      parameters,
      wordLookupParameters, charLookupParameters,
      charFwRnnBuilder, charBwRnnBuilder,
      fwBuilder, bwBuilder)

    //
    // Train the LM (optional)
    //
    // TODO

    //
    // Save the combined parameters into a single model file
    //
    val outModelFile = config.getArgString("rnnlm.train.model", None)
    val outDynetFilename = mkDynetFilename(outModelFile)
    val outX2iFilename = mkX2iFilename(outModelFile)

    new CloseableModelSaver(outDynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(parameters, "/rnnlm")
    }

    Serializer.using(LstmUtils.newPrintWriter(outX2iFilename)) { printWriter =>
      lm.saveX2i(printWriter)
    }

    logger.info("Done.")
  }
}

class RnnLMTrain
