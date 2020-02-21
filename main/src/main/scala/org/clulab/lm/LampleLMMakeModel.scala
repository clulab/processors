package org.clulab.lm

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{Dim, ParameterCollection}
import org.clulab.sequences.LstmUtils
import org.clulab.sequences.LstmUtils.{mkDynetFilename, mkX2iFilename}
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser


/**
 * Constructs the model file for the Lample et al. (2016) language model
 * This contains just static word embeddings (GloVe) + BiLSTM character embeddings that are learned with the task
 */
object LampleLMMakeModel {
  val logger:Logger = LoggerFactory.getLogger(classOf[LampleLMMakeModel])

  def main(args: Array[String]): Unit = {
    LstmUtils.initializeDyNet()
    val configName = "lample"
    val config = new FlairConfig(ConfigFactory.load(configName))

    //
    // Load the character map
    //
    logger.debug(s"Loading the character map...")
    val c2iFilename = config.getArgString("lample.merge.c2i", None)
    val c2i = Serializer.using(LstmUtils.newSource(c2iFilename)) { source =>
      val byLineCharMapBuilder = new LstmUtils.ByLineCharIntMapBuilder()
      val lines = source.getLines()
      val c2i = byLineCharMapBuilder.build(lines)
      c2i
    }
    logger.debug(s"Loaded a character map with ${c2i.keySet.size} entries.")

    //
    // load the word embeddings
    //
    logger.debug("Loading word embeddings...")
    val embedFilename = config.getArgString("lample.merge.embed", None)
    val docFreqFilename = config.getArgString("lample.merge.docFreq", None)
    val minFreq = config.getArgInt("lample.merge.minWordFreq", Some(100))
    val w2v = LstmUtils.loadEmbeddings(Some(docFreqFilename), minFreq, embedFilename)
    val w2i = LstmUtils.mkWordVocab(w2v)

    val parameters = new ParameterCollection()

    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(w2v.dimensions))
    LstmUtils.initializeEmbeddings(w2v, w2i, wordLookupParameters)
    logger.debug("Completed loading word embeddings.")

    //
    // save the combined parameters into a single model file
    //
    val outFlairModelFile = config.getArgString("lample.merge.model", None)
    val outDynetFilename = mkDynetFilename(outFlairModelFile)
    val outX2iFilename = mkX2iFilename(outFlairModelFile)

    new CloseableModelSaver(outDynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(parameters, "/lample")
    }

    Serializer.using(LstmUtils.newPrintWriter(outX2iFilename)) { printWriter =>
      LstmUtils.saveCharMap(printWriter, c2i, "c2i")
      LstmUtils.save(printWriter, w2i, "w2i")
    }
  }
}

class LampleLMMakeModel
