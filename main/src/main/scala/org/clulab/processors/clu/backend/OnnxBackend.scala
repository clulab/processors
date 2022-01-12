package org.clulab.processors.clu.backend

import ai.onnxruntime.OnnxTensor
import ai.onnxruntime.OrtEnvironment
import ai.onnxruntime.OrtSession
import org.clulab.dynet.ConstEmbeddingsGlove
import org.clulab.dynet.Utils
import org.clulab.embeddings.WordEmbeddingMap
import org.clulab.processors.clu.AnnotatedSentence
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.FileUtils
import org.json4s.JArray
import org.json4s.JInt
import org.json4s.JObject
import org.json4s.JValue
import org.json4s.jackson.JsonMethods.parse

import java.io.BufferedInputStream
import java.util.{HashMap => JHashMap}

class OnnxBackend {
  protected val wordEmbeddingMap: WordEmbeddingMap = ConstEmbeddingsGlove.SINGLETON_WORD_EMBEDDING_MAP.get
  protected val ortEnvironment: OrtEnvironment = OrtEnvironment.getEnvironment
  protected val sessionCreator = new SessionCreator(ortEnvironment)

  def toMap(jValue: JValue): Map[String, Long] = {
    jValue.asInstanceOf[JObject].obj.map { case (key: Any, value) =>
      key -> value.asInstanceOf[JInt].num.longValue()
    }.toMap
  }
}

object OnnxBackend extends CluBackend

class OnnxPosBackend(modelFilenamePrefix: String) extends PosBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      (IndexedSeq[String], IndexedSeq[String], IndexedSeq[String]) = ???  // tags, chunks, and preds
}

class OnnxNerBackend(wordModel: String, charModel: String, x2i: String) extends OnnxBackend with NerBackend {
  protected val wordSession: OrtSession = sessionCreator.create(wordModel)
  protected val charSession: OrtSession = sessionCreator.create(charModel)
  val (w2i, c2i, i2t) = {
    val json = FileUtils.getTextFromResource(x2i)
    val jArray = parse(json).asInstanceOf[JArray].arr
    val w2i = toMap(jArray(0) \ "x2i" \ "initialLayer" \ "w2i")
    val c2i = toMap(jArray(0) \ "x2i" \ "initialLayer" \ "c2i").map { case (key, value) => key.head -> value }
    val i2t = toMap(jArray(1) \ "x2i" \ "finalLayer"   \ "t2i").toArray.sortBy(_._2).map(_._1)

    (w2i, c2i, i2t)
  }

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] = {
    val words = annotatedSentence.words
    val wordEmbeddings: Array[Array[Float]] = new Array[Array[Float]](words.length)
    val wordIds: Array[Long] = new Array[Long](words.length)
    val charEmbeddings: Array[Array[Float]] = new Array[Array[Float]](words.length)

    words.indices.foreach { index =>
      val word = words(index)
      wordEmbeddings(index) = wordEmbeddingMap.getOrElseUnknown(word).toArray
      wordIds(index) = w2i.getOrElse(word, 0L)

      val charIds = word.map(c2i.getOrElse(_, 0L)).toArray
      val charInput = new JHashMap[String, OnnxTensor]()
      charInput.put("char_ids", OnnxTensor.createTensor(ortEnvironment, charIds))
      charEmbeddings(index) = charSession.run(charInput).get(0).getValue.asInstanceOf[Array[Float]]
    }

    val wordInput = new JHashMap[String, OnnxTensor]()
    wordInput.put("embed", OnnxTensor.createTensor(ortEnvironment, wordEmbeddings))
    wordInput.put("words", OnnxTensor.createTensor(ortEnvironment, wordIds))
    wordInput.put("chars", OnnxTensor.createTensor(ortEnvironment, charEmbeddings))

    val emissionScores = wordSession.run(wordInput).get(0).getValue.asInstanceOf[Array[Array[Float]]]
    val labelIds = Utils.greedyPredict(emissionScores)
    val preds = labelIds.map(i2t)

    preds
  }
}

class OnnxSrlaBackend(modelFilenamePrefix: String) extends SrlaBackend {

  def predict(taskId: Int, annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] = ???
}

class OnnxDepsBackend(modelFilenamePrefix: String) extends DepsBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[(Int, String)] = ??? // heads and labels
}

class SessionCreator(ortEnvironment: OrtEnvironment) {

  // See https://stackoverflow.com/questions/33755415/how-to-read-a-resource-file-to-a-byte-array-in-scala
  def getBytesFromResource(resourceName: String): Array[Byte] = {
    new BufferedInputStream(getClass.getResourceAsStream(resourceName)).autoClose { inputStream =>
      Stream.continually(inputStream.read).takeWhile(_ != -1).map(_.toByte).toArray
    }
  }

  def create(resourceName: String): OrtSession = {
    val model = getBytesFromResource(resourceName)
    val session = ortEnvironment.createSession(model, new OrtSession.SessionOptions)

    session
  }
}
