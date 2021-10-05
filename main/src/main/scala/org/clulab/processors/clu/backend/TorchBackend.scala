package org.clulab.processors.clu.backend

import org.clulab.processors.clu.AnnotatedSentence

object TorchBackend extends CluBackend

class TorchPosBackend(modelFilenamePrefix: String) extends PosBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      (IndexedSeq[String], IndexedSeq[String], IndexedSeq[String]) = ???  // tags, chunks, and preds
}

class TorchNerBackend(modelFilenamePrefix: String) extends NerBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] = ??? // labels
}

class TorchSrlaBackend(modelFilenamePrefix: String) extends SrlaBackend {

  def predict(taskId: Int, annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] = ??? // labels
}

class TorchDepsBackend(modelFilenamePrefix: String) extends DepsBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
    IndexedSeq[(Int, String)] = ??? // heads and labels
}
