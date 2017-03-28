package org.clulab.processors.clulab.sequences

import java.io.{File, FileReader, FileWriter, PrintWriter}
import java.util.regex.Pattern

import cc.mallet.pipe.Pipe
import cc.mallet.types.{Alphabet, Instance, InstanceList, LabelAlphabet}
import org.clulab.processors.{Document, Sentence}
import org.slf4j.{Logger, LoggerFactory}
import SequenceTagger._
import cc.mallet.fst.SimpleTagger._
import cc.mallet.pipe.iterator.LineGroupIterator

/**
  * Generic sequence tagger over words implemented using the mallet CRF
  * Author: mihais
  * Date: 3/24/17
  */
abstract class SequenceTagger[L, F] {
  def verbose = true

  def train(docs:Iterator[Document]) {

    // generate features for all sentences in all docs, and save them to disk
    val f = File.createTempFile("sequence_tagger", ".train")
    val pw = new PrintWriter(new FileWriter(f))
    for(doc <- docs; sentence <- doc.sentences) {
      // labels and features for one sentence
      val labels = labelExtractor(sentence)
      val features = (0 until sentence.size).map(featureExtractor(sentence, _)).toArray

      // save this sentence to disk; features first, then labels
      assert(features.length == labels.length)
      for(i <- labels.indices) {
        pw.println(s"${features(i).mkString(" ")} ${labels(i)}")
      }
      pw.println()
    }
    logger.debug(s"Saved training file ${f.getAbsolutePath}")
    pw.close()

    // actual CRF training
    trainCRF(f)

    // cleanup
    f.delete()
    logger.debug(s"Deleted temporary training file ${f.getAbsolutePath}")
  }

  def trainCRF(trainFile:File) {
    // read training data from file
    val pipe = new SimpleTaggerSentence2FeatureVectorSequence
    pipe.getTargetAlphabet.lookupIndex(defaultLabel)
    pipe.setTargetProcessing(true)
    val trainingData = new InstanceList(pipe)
    trainingData.addThruPipe(new LineGroupIterator(new FileReader(trainFile), Pattern.compile("^\\s*$"), true))


    if (p.isTargetProcessing) {
      val targets = p.getTargetAlphabet
      val buf = new StringBuffer("Labels:")
      var i = 0
      while (i < targets.size) buf.append(" ").append(targets.lookupObject(i).toString) {
        i += 1; i - 1
      }
      logger.info(buf.toString)
    }
    if (trainOption.value) {
      crf = train(trainingData, testData, eval,
        ordersOption.value, defaultOption.value,
        forbiddenOption.value, allowedOption.value,
        connectedOption.value, iterationsOption.value,
        gaussianVarianceOption.value, crf)
      if (modelOption.value != null) {
        ObjectOutputStream s =
          new ObjectOutputStream(new FileOutputStream(modelOption.value))
        s.writeObject(crf)
        s.close()
      }
    }
  }

  def classesOf(sentence: Sentence):Array[L] = {
    // TODO
    null
  }

  /** Abstract method that generates the features for a given sentence */
  def featureExtractor(sentence: Sentence, offset:Int):Set[F]

  /** Abstract method that extracts the training labels for a given sentence */
  def labelExtractor(sentence:Sentence): Array[L]
}

class SentenceLabelsFeatures[L, F] (val labels: Array[L], val features:Array[Set[F]]) {
  override def toString: String = {
    val b = new StringBuilder
    b.append(labels.mkString(", "))
    b.append("\n")
    b.append(features.mkString(", "))
    b.append("\n")
    b.toString()
  }
}

class ToFeatureVector extends Pipe(new Alphabet(), new LabelAlphabet())  {
  override def pipe(carrier: Instance):Instance = {
    // TODO
    null
  }
}

object SequenceTagger {
  val logger:Logger = LoggerFactory.getLogger(classOf[PartOfSpeechTagger])

  val defaultLabel = "O"
}
