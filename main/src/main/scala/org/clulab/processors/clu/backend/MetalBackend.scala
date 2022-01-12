package org.clulab.processors.clu.backend

import org.clulab.dynet.ConstEmbeddingsGlove
import org.clulab.dynet.Metal
import org.clulab.processors.Document
import org.clulab.processors.clu.AnnotatedSentence

import java.io.Closeable

object MetalBackend extends CluBackend {

  def mkEmbeddings(doc: Document): Closeable = {
    // Fetch the const embeddings from GloVe. All our models need them.
    ConstEmbeddingsGlove.mkConstLookupParams(doc)
  }
}

class MetalPosBackend(modelFilenamePrefix: String) extends PosBackend {
  protected val mtlPosChunkSrlp: Metal = Metal(modelFilenamePrefix)

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      (IndexedSeq[String], IndexedSeq[String], IndexedSeq[String]) = {
    val IndexedSeq(tags, chunks, preds, _*) = mtlPosChunkSrlp.predictJointly(annotatedSentence, embeddingsAttachment.get)

    (tags, chunks, preds)
  }
}

class MetalNerBackend(modelFilenamePrefix: String) extends NerBackend {
  protected val mtlNer: Metal = Metal(modelFilenamePrefix)

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] =
    mtlNer.predictJointly(annotatedSentence, embeddingsAttachment.get).head // labels
}

class MetalSrlaBackend(modelFilenamePrefix: String) extends SrlaBackend {
  protected val mtlSrla: Metal = Metal(modelFilenamePrefix)

  def predict(taskId: Int, annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] =
    mtlSrla.predict(taskId, annotatedSentence, embeddingsAttachment.get) // labels
}

class MetalDepsBackend(modelFilenamePrefix: String) extends DepsBackend {
  protected val mtlDeps: Metal = Metal(modelFilenamePrefix)

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[(Int, String)] =
    mtlDeps.parse(annotatedSentence, embeddingsAttachment.get) // heads and labels
}
