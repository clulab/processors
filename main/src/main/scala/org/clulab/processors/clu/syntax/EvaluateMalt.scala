package org.clulab.processors.clu.syntax

import java.io.{BufferedReader, File, FileReader}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import org.maltparser.concurrent.ConcurrentUtils
import org.maltparser.core.lw.helper.Utils
import org.slf4j.LoggerFactory

class EvaluateMalt

/**
 * Evaluates a model produced by TrainMalt
 * User: mihais
 * Date: 1/5/14
 * Last Modified: Fix compiler issue: import scala.io.Source.
 */
object EvaluateMalt {

  val logger = LoggerFactory.getLogger(classOf[EvaluateMalt])

  def main(args:Array[String]) {
    if (args.length != 2) {
      println("Usage: org.clulab.processors.clulab.syntax.EvaluateMalt <model file name> <testing treebank in conllx format>")
      System.exit(1)
    }
    val modelName = args(0)
    val testFile = args(1)

    val maltModel = new MaltWrapper(modelName)
    println(s"Successfully created malt model from $modelName.")

    val reader = new BufferedReader(new FileReader(testFile))
    evaluate(maltModel, reader)
    reader.close()
  }

  def evaluate(maltModel:Parser, reader:BufferedReader): (Double, Double) = {
    val goldDeps = new ArrayBuffer[EvalDependency]()
    val sysDeps = new ArrayBuffer[EvalDependency]()
    var done = false
    var count = 0
    val verbose = false
    logger.info("Beginning parsing...")
    while(! done) {
      val goldTokens = ConcurrentUtils.readSentence(reader)
      if(verbose) {
        println("GOLD:")
        for (t <- goldTokens) println(t)
      }
      goldDeps ++= toDeps(goldTokens)
      if(goldTokens.isEmpty) {
        done = true
      } else {
        count += 1
        val inputTokens = ConcurrentUtils.stripGold(goldTokens, 4)
        if(verbose) {
          println("INPUT TOKENS:")
          for (t <- inputTokens) println(t)
        }

        val outputTokens = maltModel.parseSentenceConllx(inputTokens)
        if(verbose) {
          println("SYS:")
          for (t <- outputTokens) println(t)
          println("\n")
        }

        sysDeps ++= toDeps(outputTokens)
        if(count % 100 == 0)
          logger.debug(s"Parsed $count sentences...")
      }
    }
    logger.info(s"Finished parsing $count sentences.")
    assert(goldDeps.size == sysDeps.size)

    val (las, uas) = score(goldDeps.toArray, sysDeps.toArray)
    println(s"LAS = $las")
    println(s"UAS = $uas")

    (las, uas)
  }

  def score(goldDeps:Array[EvalDependency], sysDeps:Array[EvalDependency]):(Double, Double) = {
    var correctLabeled = 0
    var correctUnlabeled = 0
    for(i <- goldDeps.indices) {
      val g = goldDeps(i)
      val s = sysDeps(i)
      if(g.head == s.head) {
        correctUnlabeled += 1
        if(g.label == s.label)
          correctLabeled += 1
      }
    }

    val las = correctLabeled.toDouble / goldDeps.length.toDouble
    val uas = correctUnlabeled.toDouble / goldDeps.length.toDouble
    (las, uas)
  }

  def toDeps(sentence: Array[String]):ArrayBuffer[EvalDependency] = {
    val deps = new ArrayBuffer[EvalDependency]()
    for(line <- sentence) {
      // println(s"Converting line: $line")
      val tokens = line.split("\\s+")
      if(tokens.size < 8)
        throw new RuntimeException(s"ERROR: invalid output line: $line")
      val label = tokens(7)
      val head = tokens(6).toInt
      deps += new EvalDependency(label, head)
    }
    deps
  }

  def readDependencies(fn:String):Array[EvalDependency] = {
    val deps = new ArrayBuffer[EvalDependency]()
    for(line <- Source.fromFile(fn).getLines()) {
      val content = line.trim
      if(content.length > 0) {
        val tokens = content.split("\\s+")
        if(tokens.size < 8)
          throw new RuntimeException(s"ERROR: invalid output line in file $fn: $line")
        val label = tokens(7)
        val head = tokens(6).toInt
        deps += new EvalDependency(label, head)
      }
    }
    deps.toArray
  }

}

class EvalDependency(val label:String, val head:Int)
