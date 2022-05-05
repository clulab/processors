package org.clulab.dynet

import org.clulab.embeddings.{CompactWordEmbeddingMap, WordEmbeddingMapPool}

import java.io.{FileWriter, PrintWriter}

import com.typesafe.config.ConfigFactory
import org.clulab.dynet.Utils._
import org.clulab.utils.StringUtils

import scala.io.Source
import scala.util.parsing.json._

import ai.onnxruntime.{OnnxTensor, OrtEnvironment, OrtSession}
import org.slf4j.{Logger, LoggerFactory}

import java.time.LocalDateTime
import java.time.Duration

import scala.io.Source


object TestOnnx extends App {

    def get_embeddings(embed_file_path: String): Map[String,Array[Float]]={
        val emb = Source.fromFile(embed_file_path)
        var emb_map:Map[String,Array[Float]] = Map()
        for (s<-emb.getLines){
            if (s.split(" ")(0) == ""){
                emb_map += ("<UNK>"-> s.split(" ").slice(1, s.split(" ").size).map(_.toFloat))
            }else{
                emb_map += (s.split(" ")(0) -> s.split(" ").slice(1, s.split(" ").size).map(_.toFloat))
            }
        }
        emb_map
    }
    val start_time = LocalDateTime.now()
    val props = StringUtils.argsToProperties(args)

    val configName = props.getProperty("conf")
    val config = ConfigFactory.load(configName)
    val taskManager = new TaskManager(config)
    
    val embed_file_path: String = "/data1/home/zheng/processors/main/src/main/python/glove.840B.300d.10f.txt"
    val wordEmbeddingMap = get_embeddings(embed_file_path)

    val jsonString = Source.fromFile("/data1/home/zheng/processors/main/src/main/python/ner2-epoch6.json").getLines.mkString
    val parsed = JSON.parseFull(jsonString)
    val w2i = parsed.get.asInstanceOf[List[Any]](0).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("initialLayer").asInstanceOf[Map[String, Any]]("w2i").asInstanceOf[Map[String, Double]]
    val c2i = parsed.get.asInstanceOf[List[Any]](0).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("initialLayer").asInstanceOf[Map[String, Any]]("c2i").asInstanceOf[Map[String, Double]]
    val t2i = parsed.get.asInstanceOf[List[Any]](1).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("finalLayer").asInstanceOf[Map[String, Any]]("t2i").asInstanceOf[Map[String, Double]]
    val i2t = for ((k,v) <- t2i) yield (v, k)

    val inferenceType = parsed.get.asInstanceOf[List[Any]](1).asInstanceOf[Map[String, Any]]("x2i").asInstanceOf[Map[String, Any]]("finalLayer").asInstanceOf[Map[String, Any]]("inferenceType").asInstanceOf[String]
    val ortEnvironment = OrtEnvironment.getEnvironment
    val modelpath1 = "/data1/home/zheng/processors/main/src/main/python/depsh_char.onnx"
    val mtlHeads_session1 = ortEnvironment.createSession(modelpath1, new OrtSession.SessionOptions)
    val modelpath2 = "/data1/home/zheng/processors/main/src/main/python/depsh_model.onnx"
    val mtlHeads_session2 = ortEnvironment.createSession(modelpath2, new OrtSession.SessionOptions)

    val modelpath3 = "/data1/home/zheng/processors/main/src/main/python/depsl_char.onnx"
    val mtlLabels_session1 = ortEnvironment.createSession(modelpath3, new OrtSession.SessionOptions)
    val modelpath4 = "/data1/home/zheng/processors/main/src/main/python/depsl_model.onnx"
    val mtlLabels_session2 = ortEnvironment.createSession(modelpath4, new OrtSession.SessionOptions)

    val sentences = ColumnReader.readColumns(testFile)
    println(s"Read ${sentences.length} sentences.")
    val reader = new MetalRowReader
    val eisner = new Eisner
    val scoreCountsByLabel = new ScoreCountsByLabel

    for(sentence <- sentences) {
        val annotatedSentences = reader.toAnnotatedSentences(sentence, 0)

        for(as <- annotatedSentences) {
            val annotatedSentence = as._1
            val goldLabels = as._2.map(_.label)
            val preds = eisner.ensembleParserOnONNX(mtlHeads_session1, mtlHeads_session2, 
                                                    Some(mtlLabels_session2), Some(mtlLabels_session2),
                                                    i2t,
                                                    annotatedSentence, wordEmbeddingMap, 5, 0.6f, true)
            val predLabels = preds.map(_._1.toString)

            val sc = SeqScorer.f1(goldLabels, predLabels)
            scoreCountsByLabel.incAll(sc)
        }
    }

    println(s"Accuracy on ${sentences.length} sentences: ${scoreCountsByLabel.accuracy()}")
    println(s"Precision on ${sentences.length} sentences: ${scoreCountsByLabel.precision()}")
    println(s"Recall on ${sentences.length} sentences: ${scoreCountsByLabel.recall()}")
    println(s"Micro F1 on ${sentences.length} sentences: ${scoreCountsByLabel.f1()}")
    for(label <- scoreCountsByLabel.labels) {
        println(s"\tP/R/F1 for label $label (${scoreCountsByLabel.map(label).gold}): ${scoreCountsByLabel.precision(label)} / ${scoreCountsByLabel.recall(label)} / ${scoreCountsByLabel.f1(label)}")
    }
    println(Duration.between(start_time, LocalDateTime.now()).getSeconds)

}