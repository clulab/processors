package org.clulab.processors.clu.backend

import ai.onnxruntime.OnnxTensor
import ai.onnxruntime.OrtEnvironment
import ai.onnxruntime.OrtSession
import com.typesafe.config.ConfigFactory
import org.clulab.dynet.TaskManager
import org.clulab.dynet.Utils
import org.clulab.processors.clu.AnnotatedSentence

import _root_.scala.io.Source
import _root_.scala.util.parsing.json._

object OnnxBackend extends CluBackend

class OnnxPosBackend(modelFilenamePrefix: String) extends PosBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      (IndexedSeq[String], IndexedSeq[String], IndexedSeq[String]) = ???  // tags, chunks, and preds
}

class OnnxNerBackend(modelFilenamePrefix: String) extends NerBackend {

  def get_embeddings(embed_file_path: String): Map[String,Array[Float]]={
    val emb = Source.fromFile(embed_file_path)
    var emb_map: Map[String,Array[Float]] = Map()
    for (s <- emb.getLines) {
      if (s.split(" ")(0) == ""){
        emb_map += ("<UNK>"-> s.split(" ").slice(1, s.split(" ").size).map(_.toFloat))
      }else{
        emb_map += (s.split(" ")(0) -> s.split(" ").slice(1, s.split(" ").size).map(_.toFloat))
      }
    }
    emb_map
  }

  val configName = "org/clulab/mtl-en-ner.conf"

  val config = ConfigFactory.load(configName)
  val taskManager = new TaskManager(config)

  val embed_file_path: String = "../glove/glove.840B.300d.10f.txt"
  val wordEmbeddingMap = get_embeddings(embed_file_path)

  val jsonString = Source.fromFile("../onnx/ner.json").getLines.mkString
  val parsed = JSON.parseFull(jsonString)
  val w2i = parsed.get.asInstanceOf[List[Any]](0).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("initialLayer").asInstanceOf[Map[String, Any]]("w2i").asInstanceOf[Map[String, Double]]
  val c2i = parsed.get.asInstanceOf[List[Any]](0).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("initialLayer").asInstanceOf[Map[String, Any]]("c2i").asInstanceOf[Map[String, Double]]
  val t2i = parsed.get.asInstanceOf[List[Any]](1).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("finalLayer").asInstanceOf[Map[String, Any]]("t2i").asInstanceOf[Map[String, Double]]
  val i2t = for ((k,v) <- t2i) yield (v, k)

  val ortEnvironment = OrtEnvironment.getEnvironment
  val modelpath1 = "../onnx/char.onnx"
  val session1 = ortEnvironment.createSession(modelpath1, new OrtSession.SessionOptions)
  val modelpath2 = "../onnx/model.onnx"
  val session2 = ortEnvironment.createSession(modelpath2, new OrtSession.SessionOptions)

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] = {

    val words = annotatedSentence.words
    val embeddings: Array[Array[Float]] = new Array[Array[Float]](words.length)
    val wordIds: Array[Long] = new Array[Long](words.length)
    val char_embs: Array[Array[Float]] = new Array[Array[Float]](words.length)

    for (i <- words.indices){
      val word = words(i)
      embeddings(i) = wordEmbeddingMap.getOrElse(word,wordEmbeddingMap.get( "<UNK>").get)
      wordIds(i) = w2i.getOrElse(word, 0).asInstanceOf[Number].longValue
      val char_input = new java.util.HashMap[String, OnnxTensor]()
      char_input.put("char_ids",  OnnxTensor.createTensor(ortEnvironment, word.map(c => c2i.getOrElse(c.toString, 0).asInstanceOf[Number].longValue).toArray))
      char_embs(i) = session1.run(char_input).get(0).getValue.asInstanceOf[Array[Float]]
    }

    val input = new java.util.HashMap[String, OnnxTensor]()
    val emb_tensor = OnnxTensor.createTensor(ortEnvironment, embeddings)
    input.put("embed", emb_tensor)
    val word_tensor = OnnxTensor.createTensor(ortEnvironment, wordIds)
    input.put("words", word_tensor)
    val char_tensor = OnnxTensor.createTensor(ortEnvironment, char_embs)
    input.put("chars", char_tensor)
    val emissionScores = session2.run(input).get(0).getValue.asInstanceOf[Array[Array[Float]]]
    val labelIds = Utils.greedyPredict(emissionScores)
    val preds = labelIds.map(i2t(_))

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
