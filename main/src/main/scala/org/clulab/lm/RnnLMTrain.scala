package org.clulab.lm

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{Dim, LstmBuilder, ParameterCollection}
import org.clulab.sequences.LstmUtils
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

/**
 * Constructs the RnnLM model
 * This starts similarly to Lample et al. (2016), but it includes a LM training component as well
 * This contains static word embeddings (GloVe) + biLSTM character embeddings that are learned with the task,
 *   which then feed into a word-level biLSTM
 */
object RnnLMTrain {
  val logger:Logger = LoggerFactory.getLogger(classOf[RnnLMTrain])

  def main(args: Array[String]): Unit = {
    LstmUtils.initializeDyNet() // autoBatch = true, mem = "1660,1664,2496,1400")
    val configName = "rnnlm-en"
    val config = new FlairConfig(ConfigFactory.load(configName))

    val charEmbeddingSize = config.getArgInt("rnnlm.train.charEmbeddingSize", Some(32))
    val charRnnStateSize = config.getArgInt("rnnlm.train.charRnnStateSize", Some(16))
    val wordRnnStateSize = config.getArgInt("rnnlm.train.wordRnnStateSize", Some(256))

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
    val minMandatoryFreq = config.getArgInt("rnnlm.train.minMandatoryWordFreq", Some(1))
    val w2v = LstmUtils.loadEmbeddings(
      Some(docFreqFilename), minFreq, embedFilename,
      Some(config.getArgString("rnnlm.train.mandatoryWords", None)), minMandatoryFreq)
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
    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(charEmbeddingSize))
    val charFwRnnBuilder = new LstmBuilder(1, charEmbeddingSize, charRnnStateSize, parameters)
    val charBwRnnBuilder = new LstmBuilder(1, charEmbeddingSize, charRnnStateSize, parameters)

    val embeddingSize = 2 * charRnnStateSize + w2v.dimensions + 1 // 1 for isPredFeature
    val fwBuilder = new LstmBuilder(1, embeddingSize, wordRnnStateSize, parameters)
    val bwBuilder = new LstmBuilder(1, embeddingSize, wordRnnStateSize, parameters)

    //
    // Feed forward networks, used for LM prediction, only if training is enabled
    //
    val lmLabelCount = config.getArgInt("rnnlm.train.lmLabelCount", Some(40000)) + 2 // + 2 for UNK and EOS
    val fwO = parameters.addParameters(Dim(lmLabelCount, wordRnnStateSize))
    val bwO = parameters.addParameters(Dim(lmLabelCount, wordRnnStateSize))

    //
    // Create the LM object
    //
    val lm = new RnnLM(w2i, c2i,
      wordRnnStateSize, charRnnStateSize, lmLabelCount,
      parameters,
      wordLookupParameters, charLookupParameters,
      charFwRnnBuilder, charBwRnnBuilder,
      fwBuilder, bwBuilder, fwO, bwO)

    //
    // Train the LM (optional)
    //
    val doTrain = config.getArgBoolean("rnnlm.train.doTrain", Some(false))
    if(doTrain && config.contains("rnnlm.train.train")) {
      lm.trainLM(
        config.getArgString("rnnlm.train.train", None),
        Some(config.getArgString("rnnlm.train.dev", None)),
        lmLabelCount,
        config.getArgInt("rnnlm.train.logCheckpoint", Some(1000)),
        config.getArgInt("rnnlm.train.saveCheckpoint", Some(50000)),
        config.getArgInt("rnnlm.train.batchSize", Some(1))
      )
    }

    //
    // Save the combined parameters into a single model file
    //
    val outModelFile = config.getArgString("rnnlm.train.model", None)
    lm.save(outModelFile)

    logger.info("Done.")
  }
}

class RnnLMTrain
