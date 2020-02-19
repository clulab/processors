package org.clulab.ctxemb

import com.typesafe.config.ConfigFactory
import org.clulab.ctxemb.FlairParameters.mkParams
import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.sequences.{ArrayMath, LstmUtils}
import org.clulab.sequences.LstmUtils.{initializeDyNet, mkDynetFilename, mkX2iFilename}
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}
import edu.cmu.dynet._
import edu.cmu.dynet.Expression._
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser


import scala.collection.mutable.ListBuffer

/**
 * Merges the Flair character LM parameters with word embeddings into a single model file
 */
object FlairWordMerge {
  val logger:Logger = LoggerFactory.getLogger(classOf[FlairWordMerge])

  def main(args: Array[String]): Unit = {
    initializeDyNet() // autoBatch = true, mem = "512")
    val configName = "flair"
    val config = new FlairConfig(ConfigFactory.load(configName))

    //
    // load the Flair trained parameters
    //
    logger.debug("Loading the Flair character LM model...")
    val inputFlairModelFile = config.getArgString("flair.merge.model.input", None)
    val dynetFilename = mkDynetFilename(inputFlairModelFile)
    val x2iFilename = mkX2iFilename(inputFlairModelFile)

    val (c2i, dim) = Serializer.using(LstmUtils.newSource(x2iFilename)) { source =>
      val byLineCharMapBuilder = new LstmUtils.ByLineCharIntMapBuilder()
      val lines = source.getLines()
      val c2i = byLineCharMapBuilder.build(lines)
      val dim = new LstmUtils.ByLineIntBuilder().build(lines)
      (c2i, dim)
    }
    logger.debug(s"Loaded a character map with ${c2i.keySet.size} entries.")

    val model = {
      val model = mkParams(c2i)
      LstmUtils.loadParameters(dynetFilename, model.parameters, key = "/flair")
      model
    }
    logger.debug("Completed loading the Flair character LM.")

    //
    // load the word embeddings
    //
    logger.debug("Loading word embeddings...")
    val embedFilename = config.getArgString("flair.merge.embed", None)
    val docFreqFilename = config.getArgString("flair.merge.docFreq", None)
    val minFreq = config.getArgInt("flair.merge.minWordFreq", Some(100))
    val w2v = LstmUtils.loadEmbeddings(Some(docFreqFilename), minFreq, embedFilename)
    val w2i = mkWordVocab(w2v)

    val wordLookupParameters = model.parameters.addLookupParameters(w2i.size, Dim(w2v.dimensions))
    initializeEmbeddings(w2v, w2i, wordLookupParameters)
    logger.debug("Completed loading word embeddings.")

    //
    // save the combined parameters into a single model file
    //
    val outFlairModelFile = config.getArgString("flair.merge.model.output", None)
    val outDynetFilename = mkDynetFilename(outFlairModelFile)
    val outX2iFilename = mkX2iFilename(outFlairModelFile)

    new CloseableModelSaver(outDynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(model.parameters, "/flair")
    }

    Serializer.using(LstmUtils.newPrintWriter(outX2iFilename)) { printWriter =>
      val dim = model.charLookupParameters.dim().get(0)

      LstmUtils.saveCharMap(printWriter, c2i, "c2i")
      LstmUtils.save(printWriter, dim, "dim")
      LstmUtils.save(printWriter, w2i, "w2i")
    }
  }

  private def mkWordVocab(w2v:Word2Vec): Map[String, Int] = {
    val commonWords = new ListBuffer[String]
    commonWords += LstmUtils.UNK_WORD // the word at position 0 is reserved for unknown words
    for(w <- w2v.matrix.keySet.toList.sorted) {
      commonWords += w
    }
    val w2i = commonWords.zipWithIndex.toMap
    w2i
  }

  def initializeEmbeddings(w2v:Word2Vec, w2i:Map[String, Int], lookupParameters: LookupParameter): Unit = {
    logger.debug("Initializing DyNet embedding parameters...")
    for(word <- w2v.matrix.keySet){
      lookupParameters.initialize(w2i(word), new FloatVector(ArrayMath.toFloatArray(w2v.matrix(word))))
    }
    logger.debug(s"Completed initializing embedding parameters for a vocabulary of size ${w2v.matrix.size}.")
  }
}

class FlairWordMerge
