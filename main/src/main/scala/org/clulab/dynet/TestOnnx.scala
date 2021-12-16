package org.clulab.dynet

import org.clulab.dynet.ConstEmbeddingsGlove
import org.clulab.embeddings.WordEmbeddingMapPool

import java.io.{FileWriter, PrintWriter}

import com.typesafe.config.ConfigFactory
import org.clulab.dynet.Utils._
import org.clulab.utils.StringUtils


object GetWordEmbeddings extends App {
    val props = StringUtils.argsToProperties(args)

    val configName = props.getProperty("conf")
    val config = ConfigFactory.load(configName)
    val taskManager = new TaskManager(config)
    val constEmbeddingsGlove = ConstEmbeddingsGlove // Make sure that the embeddings have been loaded.
    val wordEmbeddingMap = WordEmbeddingMapPool.get("glove.840B.300d.10f", compact = true).get
    for(taskId <- 0 until taskManager.taskCount) {
        val taskName = taskManager.tasks(taskId).taskName
        val testSentences = taskManager.tasks(taskId).testSentences
        val testSentences = taskManager.tasks(taskId).testSentences
        val taskNumber = taskId + 1

        if(testSentences.nonEmpty){
            val scoreCountsByLabel = new ScoreCountsByLabel
            val reader = new MetalRowReader
            for (sent <- testSentences) {
                val annotatedSentences = reader.toAnnotatedSentences(sent)
                for(as <- annotatedSentences) {
                    val sentence = as._1
                    val goldLabels = as._2

                    // val sc = SeqScorer.f1(goldLabels, preds)
                    // scoreCountsByLabel.incAll(sc)
                }
            }
        }
        // logger.info(s"Accuracy on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.accuracy()}")
        // logger.info(s"Precision on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.precision()}")
        // logger.info(s"Recall on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.recall()}")
        // logger.info(s"Micro F1 on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.f1()}")
        // for(label <- scoreCountsByLabel.labels) {
        //     logger.info(s"\tP/R/F1 for label $label (${scoreCountsByLabel.map(label).gold}): ${scoreCountsByLabel.precision(label)} / ${scoreCountsByLabel.recall(label)} / ${scoreCountsByLabel.f1(label)}")
        // }
    }

}