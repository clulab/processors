package edu.arizona.sista.processors.corenlp.chunker

import java.io.File
import java.util.Properties
import scala.collection.JavaConverters._
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.ling.{ CoreLabel, CoreAnnotations }

class CRFChunker(crf: CRFClassifier[CoreLabel]) {

  import CRFChunker.mkCoreLabels

  def save(path: String): Unit =
    crf.serializeClassifier(path)

  def classify(words: Array[String], tags: Array[String]): Array[String] =
    classify(mkCoreLabels(words, tags))

  def classify(labels: Seq[CoreLabel]): Array[String] = {
    val predictions = crf.classify(labels.toBuffer.asJava)
    val chunks = predictions.asScala.map(_.getString(classOf[CoreAnnotations.AnswerAnnotation]))
    chunks.toArray
  }

}

object CRFChunker {

  def load(path: String): CRFChunker =
    load(new File(path))

  def load(file: File): CRFChunker = {
    val crf = mkClassifier()
    crf.loadClassifier(file)
    new CRFChunker(crf)
  }

  def train(sentences: Array[Array[CoreLabel]]): CRFChunker = {
    // calling toBuffer is necessary because List is immutable
    // and corenlp tries to modify the sequence in place :(
    val data = sentences.map(_.toBuffer.asJava).toBuffer.asJava
    val crf = mkClassifier()
    crf.train(data)
    new CRFChunker(crf)
  }

  def mkClassifier(): CRFClassifier[CoreLabel] = {
    val props = new Properties()
    props.setProperty("macro", "true") // use a generic CRF configuration
    props.setProperty("featureFactory", "edu.arizona.sista.chunker.ChunkingFeatureFactory") // our own FF!
    new CRFClassifier[CoreLabel](props)
  }

  def mkCoreLabels(words: Array[String], tags: Array[String]): Array[CoreLabel] =
    words zip tags map { case (word, tag) =>
      val label = new CoreLabel
      label.setWord(word)
      label.setTag(tag)
      label
    }

}
