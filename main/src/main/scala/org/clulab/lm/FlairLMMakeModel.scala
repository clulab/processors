package org.clulab.lm

import com.typesafe.config.ConfigFactory
import org.clulab.sequences.LstmUtils
import org.clulab.sequences.LstmUtils.{initializeDyNet, mkDynetFilename, mkX2iFilename}
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}
import edu.cmu.dynet._
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser

/**
 * Merges the Flair character LM parameters (produced by FlairTrainer) with word embeddings (GloVe) into a model file
 */
object FlairLMMakeModel {
  val logger:Logger = LoggerFactory.getLogger(classOf[FlairLMMakeModel])

  def main(args: Array[String]): Unit = {
    initializeDyNet() // autoBatch = true, mem = "512")
    val configName = "flair-en"
    val config = new FlairConfig(ConfigFactory.load(configName))

    //
    // load the Flair trained parameters
    //
    logger.debug("Loading the Flair character LM model...")
    val inputFlairModelFile = config.getArgString("flair.merge.model.input", None)
    val flairTrainer = FlairTrainer(inputFlairModelFile)
    logger.debug("Completed loading the Flair character LM.")

    //
    // load the word embeddings
    //
    logger.debug("Loading word embeddings...")
    val embedFilename = config.getArgString("flair.merge.embed", None)
    val docFreqFilename = config.getArgString("flair.merge.docFreq", None)
    val minFreq = config.getArgInt("flair.merge.minWordFreq", Some(100))
    val w2v = LstmUtils.loadEmbeddings(Some(docFreqFilename), minFreq, embedFilename, None, 1)
    val w2i = LstmUtils.mkWordVocab(w2v)

    // create lookup parameters for these word embeddings; add them to the same model
    val wordLookupParameters = flairTrainer.parameters.addLookupParameters(w2i.size, Dim(w2v.dimensions))
    LstmUtils.initializeEmbeddings(w2v, w2i, wordLookupParameters)
    logger.debug("Completed loading word embeddings.")

    //
    // save the combined parameters into a single model file
    //
    val outFlairModelFile = config.getArgString("flair.merge.model.output", None)
    val outDynetFilename = mkDynetFilename(outFlairModelFile)
    val outX2iFilename = mkX2iFilename(outFlairModelFile)

    new CloseableModelSaver(outDynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(flairTrainer.parameters, "/flair")
    }

    Serializer.using(LstmUtils.newPrintWriter(outX2iFilename)) { printWriter =>
      val charDim = flairTrainer.charLookupParameters.dim().get(0)
      val wordDim = wordLookupParameters.dim().get(0)

      LstmUtils.saveCharMap(printWriter, flairTrainer.c2i, "c2i")
      LstmUtils.save(printWriter, charDim, "charDim")
      LstmUtils.save(printWriter, w2i, "w2i")
      LstmUtils.save(printWriter, wordDim, "wordDim")

    }
  }
}

class FlairLMMakeModel
