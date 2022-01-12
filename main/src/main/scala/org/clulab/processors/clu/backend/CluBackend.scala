package org.clulab.processors.clu.backend

import org.clulab.processors.Document
import org.clulab.processors.IntermediateDocumentAttachment
import org.clulab.processors.clu.AnnotatedSentence

import java.io.Closeable

case class CloseableNone() extends Closeable {
  def close(): Unit = ()
}

trait CluBackend

class EmbeddingsAttachment(protected val value: Closeable) extends IntermediateDocumentAttachment with Closeable {
  // TODO: This would need to change if multiple values were to be stored.

  def get[T]: T = value.asInstanceOf[T] // Use caution!

  def close(): Unit = value.close()
}

trait PosBackend {
  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      (IndexedSeq[String], IndexedSeq[String], IndexedSeq[String]) // tags, chunks, and preds
}

trait NerBackend {
  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] // labels
}

trait SrlaBackend {
  def predict(taskId: Int, annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] // labels
}

trait DepsBackend {
  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[(Int, String)] // heads and labels
}
