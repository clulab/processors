package org.clulab.processors.clu.backend

import org.clulab.processors.clu.AnnotatedSentence

object ScalaBackend extends CluBackend

class ScalaPosBackend(modelFilenamePrefix: String) extends PosBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      (IndexedSeq[String], IndexedSeq[String], IndexedSeq[String]) = ???  // tags, chunks, and preds
}

class ScalaNerBackend(modelFilenamePrefix: String) extends NerBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] = ??? // labels
}

class ScalaSrlaBackend(modelFilenamePrefix: String) extends SrlaBackend {

  def predict(taskId: Int, annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] = ??? // labels
}

class ScalaDepsBackend(modelFilenamePrefix: String) extends DepsBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
    IndexedSeq[(Int, String)] = ??? // heads and labels
}
