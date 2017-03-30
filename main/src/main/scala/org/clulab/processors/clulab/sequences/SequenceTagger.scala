package org.clulab.processors.clulab.sequences

import java.io._
import java.util.regex.Pattern

import cc.mallet.pipe.Pipe
import cc.mallet.types.{Alphabet, Instance, InstanceList, LabelAlphabet}
import org.clulab.processors.{Document, Sentence}
import org.slf4j.{Logger, LoggerFactory}
import SequenceTagger._
import cc.mallet.fst.{CRF, CRFTrainerByThreadedLabelLikelihood, Transducer}
import cc.mallet.fst.SimpleTagger._
import cc.mallet.pipe.iterator.LineGroupIterator

/**
  * Generic sequence tagger over words implemented using the mallet CRF
  * Author: mihais
  * Date: 3/24/17
  */
abstract class SequenceTagger[L, F] {
  def verbose = true

  var crfModel:Option[CRF] = None

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
    logger.info(s"Saved training file ${f.getAbsolutePath}")
    pw.close()

    // actual CRF training
    trainCRF(f)

    // cleanup
    f.delete()
    logger.info(s"Deleted temporary training file ${f.getAbsolutePath}")
  }

  def trainCRF(trainFile:File):Boolean = {
    // read training data from file
    val pipe = new SimpleTaggerSentence2FeatureVectorSequence
    pipe.getTargetAlphabet.lookupIndex(defaultLabel)
    pipe.setTargetProcessing(true)
    val trainingData = new InstanceList(pipe)
    trainingData.addThruPipe(new LineGroupIterator(new FileReader(trainFile), Pattern.compile("^\\s*$"), true))

    // some logging
    if (pipe.isTargetProcessing) {
      val targets = pipe.getTargetAlphabet
      val buf = new StringBuilder("Training for labels:")
      for (i <- 0 until targets.size) buf.append(" " + targets.lookupObject(i).toString)
      logger.info(buf.toString)
    }

    // initialize the CRF
    val crf = new CRF(trainingData.getPipe, null.asInstanceOf[Pipe])
    val startName = crf.addOrderNStates(trainingData, orders, null,
      defaultLabel, forbiddenPattern, allowedPattern, fullyConnected)
    for (i <- 0 until crf.numStates()) {
      crf.getState(i).setInitialWeight(Transducer.IMPOSSIBLE_WEIGHT)
    }
    crf.getState(startName).setInitialWeight(0.0)
    logger.info(s"Training on ${trainingData.size()} instances.")

    // the actual training
    val crft = new CRFTrainerByThreadedLabelLikelihood(crf, numThreads)
    crft.setGaussianPriorVariance(gaussianVariance)
    crft.setUseSparseWeights(true)
    crft.setUseSomeUnsupportedTrick(true)
    // these 2 lines correspond to the "some-dense" SimpleTagger option
    var converged = false
    for (i <- 1 to iterations if !converged) {
      logger.info(s"Training iteration #$i...")
      converged = crft.train(trainingData, 1)
    }
    crft.shutdown()

    // keep the model
    crfModel = Some(crf)
    true
  }
  
  def classesOf(sentence: Sentence):Array[L] = {
    // TODO
    null
  }

  /** Abstract method that generates the features for a given sentence */
  def featureExtractor(sentence: Sentence, offset:Int):Set[F]

  /** Abstract method that extracts the training labels for a given sentence */
  def labelExtractor(sentence:Sentence): Array[L]

  def save(fn:File) {
    assert(crfModel.isDefined)
    val os = new ObjectOutputStream(new FileOutputStream(fn))
    os.writeObject(crfModel.get)
    os.close()
  }
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

  //
  // Default options taken from SimpleTagger
  //
  val defaultLabel = "O"
  // label1,label2 transition forbidden if it matches this
  val forbiddenPattern = Pattern.compile("\\s")
  // label1,label2 transition allowed only if it matches this
  val allowedPattern = Pattern.compile(".*")
  // list of label Markov orders (main and backoff)
  val orders = Array(1)
  // number of training iterations
  val iterations = 500
  // include all allowed transitions, even those not in training data
  val fullyConnected = true
  // the gaussian prior variance used for training
  val gaussianVariance = 10.0
  // how many threads to use during training
  val numThreads = 2
  
}
