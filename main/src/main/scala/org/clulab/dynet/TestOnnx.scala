package org.clulab.dynet

import ai.onnxruntime.{OnnxTensor, OrtEnvironment, OrtSession}
import com.typesafe.config.ConfigFactory
import org.clulab.utils.{StringUtils, Timer}

import scala.io.Source
import scala.util.parsing.json._
import java.time.LocalDateTime
import java.time.Duration

object TestOnnx extends App {

  class TextEmbedder(filename: String) {

    def get_embeddings(embed_file_path: String): Map[String,Array[Float]]={
      val emb = Source.fromFile(embed_file_path)
      var emb_map:Map[String,Array[Float]] = Map()
      for (s<-emb.getLines){
        if (s.split(" ")(0) == ""){
          // TODO: These probably need to be normalized in both cases.
          emb_map += ("<UNK>"-> s.split(" ").slice(1, s.split(" ").size).map(_.toFloat))
        }else{
          emb_map += (s.split(" ")(0) -> s.split(" ").slice(1, s.split(" ").size).map(_.toFloat))
        }
      }
      emb_map
    }

    protected val map = {
      val timer = new Timer("get_embeddings")
      val result = timer.time(get_embeddings(filename))

      println(timer.toString)
      result
    }
    protected val unknown = map("<UNK>")

    def apply(key: String): Array[Float] = map.getOrElse(key, unknown)
  }

  class ProcessorsEmbedder() {
    val map = {
      val timer = new Timer("SINGLETON_WORD_EMBEDDING_MAP")
      val result = timer.time(ConstEmbeddingsGlove.SINGLETON_WORD_EMBEDDING_MAP.get)

      println(timer.toString)
      result
    }

    def apply(key: String): Array[Float] = map.getOrElseUnknown(key).toArray
  }

    val start_time = LocalDateTime.now()
    val props = StringUtils.argsToProperties(args)

    val configName = props.getProperty("conf")
    val config = ConfigFactory.load(configName)
    val taskManager = new TaskManager(config)

    // Pick one of these.
    val embedder = new TextEmbedder("/data1/home/zheng/processors/main/src/main/python/glove.840B.300d.10f.txt")
    // val embedder = new TextEmbedder("../glove/glove.840B.300d.10f.txt")
    // val embedder = new ProcessorsEmbedder()

    val jsonString = Source.fromFile("/data1/home/zheng/processors/ner.json").getLines.mkString
    // val jsonString = Source.fromFile("../onnx/ner.json").getLines.mkString
    val parsed = JSON.parseFull(jsonString)
    val w2i = parsed.get.asInstanceOf[List[Any]](0).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("initialLayer").asInstanceOf[Map[String, Any]]("w2i").asInstanceOf[Map[String, Double]]
    val c2i = parsed.get.asInstanceOf[List[Any]](0).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("initialLayer").asInstanceOf[Map[String, Any]]("c2i").asInstanceOf[Map[String, Double]]
    val t2i = parsed.get.asInstanceOf[List[Any]](1).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("finalLayer").asInstanceOf[Map[String, Any]]("t2i").asInstanceOf[Map[String, Double]]
    val i2t = for ((k,v) <- t2i) yield (v, k)

    val ortEnvironment = OrtEnvironment.getEnvironment
    val modelpath1 = "/data1/home/zheng/processors/char.onnx"
    /// val modelpath1 = "../onnx/char.onnx"
    val session1 = ortEnvironment.createSession(modelpath1, new OrtSession.SessionOptions)
    val modelpath2 = "/data1/home/zheng/processors/model.onnx"
    /// val modelpath2 = "../onnx/model.onnx"
    val session2 = ortEnvironment.createSession(modelpath2, new OrtSession.SessionOptions)

    println(session1.getOutputInfo)
    println(session2.getOutputInfo)
    
    var p = 0
    for(taskId <- 0 until taskManager.taskCount) {
        val taskName = taskManager.tasks(taskId).taskName
        val testSentences = taskManager.tasks(taskId).testSentences.get
        val taskNumber = taskId + 1

        if(testSentences.nonEmpty){
            val scoreCountsByLabel = new ScoreCountsByLabel
            val reader = new MetalRowReader
            for (sent <- testSentences) {
                val annotatedSentences = reader.toAnnotatedSentences(sent)
                for(as <- annotatedSentences) {
                    val sentence = as._1
                    val goldLabels = as._2

                    val words = sentence.words
                    var embeddings:Array[Array[Float]] = new Array[Array[Float]](words.length)
                    var wordIds:Array[Long] = new Array[Long](words.length)
                    var char_embs:Array[Array[Float]] = new Array[Array[Float]](words.length)
                    for(i <- words.indices){
                        val word = words(i)
                        embeddings(i) = embedder(word)
                        wordIds(i) = w2i.getOrElse(word, 0).asInstanceOf[Number].longValue
                        val char_input = new java.util.HashMap[String, OnnxTensor]()
                        char_input.put("char_ids",  OnnxTensor.createTensor(ortEnvironment, word.map(c => c2i.getOrElse(c.toString, 0).asInstanceOf[Number].longValue).toArray))
                        char_embs(i) = session1.run(char_input).get(0).getValue.asInstanceOf[Array[Float]]
                    }
                    
                    val input = new java.util.HashMap[String, OnnxTensor]()
                    val emb_tensor =  OnnxTensor.createTensor(ortEnvironment, embeddings)
                    input.put("embed", emb_tensor)
                    val word_tensor =  OnnxTensor.createTensor(ortEnvironment, wordIds)
                    input.put("words", word_tensor)
                    val char_tensor =  OnnxTensor.createTensor(ortEnvironment, char_embs)
                    input.put("chars", char_tensor)
                    val emissionScores = session2.run(input).get(0).getValue.asInstanceOf[Array[Array[Float]]]
                    val labelIds = Utils.greedyPredict(emissionScores)
                    val preds = labelIds.map(i2t(_))
                    val sc = SeqScorer.f1(goldLabels, preds)
                    scoreCountsByLabel.incAll(sc)
                }
            }
            println(s"Accuracy on ${testSentences.length} sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.accuracy()}")
            println(s"Precision on ${testSentences.length} sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.precision()}")
            println(s"Recall on ${testSentences.length} sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.recall()}")
            println(s"Micro F1 on ${testSentences.length} sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.f1()}")
            for(label <- scoreCountsByLabel.labels) {
                println(s"\tP/R/F1 for label $label (${scoreCountsByLabel.map(label).gold}): ${scoreCountsByLabel.precision(label)} / ${scoreCountsByLabel.recall(label)} / ${scoreCountsByLabel.f1(label)}")
            }
        }
    }
    println(Duration.between(start_time, LocalDateTime.now()).getSeconds)

}