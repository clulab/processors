package org.clulab.dynet

import org.clulab.dynet.ConstEmbeddingsGlove
import org.clulab.embeddings.WordEmbeddingMapPool

import java.io.{FileWriter, PrintWriter}

import com.typesafe.config.ConfigFactory
import org.clulab.dynet.Utils._
import org.clulab.utils.StringUtils

import scala.io.Source
import scala.util.parsing.json._

import ai.onnxruntime.{OnnxTensor, OrtEnvironment, OrtSession}


object TestOnnx extends App {
    val props = StringUtils.argsToProperties(args)

    val configName = props.getProperty("conf")
    val config = ConfigFactory.load(configName)
    val taskManager = new TaskManager(config)
    
    val constEmbeddingsGlove = ConstEmbeddingsGlove // Make sure that the embeddings have been loaded.
    val wordEmbeddingMap = WordEmbeddingMapPool.get("glove.840B.300d.10f", compact = true).get
    
    val jsonString = Source.fromFile("ner.json").getLines.mkString
    val parsed = JSON.parseFull(jsonString)
    val w2i = parsed.get.asInstanceOf[List[Any]](0).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("initialLayer").asInstanceOf[Map[String, Any]]("w2i").asInstanceOf[Map[String, Double]]
    val c2i = parsed.get.asInstanceOf[List[Any]](0).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("initialLayer").asInstanceOf[Map[String, Any]]("c2i").asInstanceOf[Map[Char, Double]]

    val ortEnvironment = OrtEnvironment.getEnvironment
    val modelpath1 = "char.onnx"
    val session1 = ortEnvironment.createSession(modelpath1, new OrtSession.SessionOptions)
    val modelpath2 = "model.onnx"
    val session2 = ortEnvironment.createSession(modelpath2, new OrtSession.SessionOptions)

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
                    var embeddings:Array[Array[Float]] = new Array[Long](words.length)
                    var wordIds:Array[Long] = new Array[Long](words.length)
                    var char_embs:Array[Array[Float]] = new Array[Array[Float]](words.length)
                    for(i <- words.indices){
                        val word = words(i)
                        embeddings(i) = wordEmbeddingMap.get(word).get.toArray
                        wordIds(i) = w2i(word).toLong
                        val char_input = new java.util.HashMap[String, OnnxTensor]()
                        char_input.put("char_ids",  OnnxTensor.createTensor(ortEnvironment, word.map(c => c2i(c.toString).toLong).toArray))
                        char_embs(i) = session1.run(char_input).get(0).getValue.asInstanceOf[Array[Float]]
                    }
                    val input = new java.util.HashMap[String, OnnxTensor]()
                    val emb_tensor =  OnnxTensor.createTensor(ortEnvironment, embeddings)
                    input.put("embed", emb_tensor)
                    val word_tensor =  OnnxTensor.createTensor(ortEnvironment, wordIds)
                    input.put("words", word_tensor)
                    val char_tensor =  OnnxTensor.createTensor(ortEnvironment, char_embs)
                    input.put("chars", char_tensor)
                    val emissionScores = session2.run(input)
                    val labelIds = Utils.greedyPredict(emissionScores)
                    val preds = labelIds.map(i2t(_))
                    val sc = SeqScorer.f1(goldLabels, preds)
                    scoreCountsByLabel.incAll(sc)
                }
            }
        }
        logger.info(s"Accuracy on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.accuracy()}")
        logger.info(s"Precision on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.precision()}")
        logger.info(s"Recall on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.recall()}")
        logger.info(s"Micro F1 on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.f1()}")
        for(label <- scoreCountsByLabel.labels) {
            logger.info(s"\tP/R/F1 for label $label (${scoreCountsByLabel.map(label).gold}): ${scoreCountsByLabel.precision(label)} / ${scoreCountsByLabel.recall(label)} / ${scoreCountsByLabel.f1(label)}")
        }
    }

}