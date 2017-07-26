package org.clulab.processors.clu.syntax

import java.io.{BufferedReader, File, FileReader}

import org.maltparser.concurrent.{ConcurrentMaltParserModel, ConcurrentMaltParserService, ConcurrentUtils}
import org.maltparser.core.lw.helper.Utils
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

class EvaluateMalt

/**
 * Evaluates a model produced by TrainMalt
 * User: mihais
 * Date: 1/5/14
 */
object EvaluateMalt {

  val logger = LoggerFactory.getLogger(classOf[EvaluateMalt])
  
  def mkMaltModel(modelName:String): ConcurrentMaltParserModel = {
    val modelURL = new File(modelName).toURI.toURL
    println(s"modelURL: $modelURL")
    val parserModelName = Utils.getInternalParserModelName(modelURL)
    println(s"parserModelName: $parserModelName")
    ConcurrentMaltParserService.initializeParserModel(modelURL)
  }

  def main(args:Array[String]) {
    if (args.length != 2) {
      println("Usage: org.clulab.processors.clulab.syntax.EvaluateMalt <model file name> <testing treebank in conllx format>")
      System.exit(1)
    }
    val modelName = args(0)
    val testFile = args(1)

    val maltModel = mkMaltModel(modelName)
    println(s"Successfully created malt model from $modelName.")

    val reader = new BufferedReader(new FileReader(testFile))
    evaluate(maltModel, reader)
    reader.close()
  }

  def evaluate(maltModel:ConcurrentMaltParserModel, reader:BufferedReader): (Double, Double) = {
    val goldDeps = new ArrayBuffer[Dependency]()
    val sysDeps = new ArrayBuffer[Dependency]()
    var done = false
    var count = 0
    logger.info("Beginning parsing...")
    while(! done) {
      val goldTokens = ConcurrentUtils.readSentence(reader)
      //println("GOLD:")
      //for(t <- goldTokens) println(t)
      goldDeps ++= toDeps(goldTokens)
      if(goldTokens.isEmpty) {
        done = true
      } else {
        count += 1
        val inputTokens = ConcurrentUtils.stripGold(goldTokens, 4)
        // for(t <- inputTokens) println(t)
        val outputTokens = maltModel.parseTokens(inputTokens)
        //println("SYS:")
        //for(t <- outputTokens) println(t)
        //println("\n")
        //if(count == 2) System.exit(0)
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

  def score(goldDeps:Array[Dependency], sysDeps:Array[Dependency]):(Double, Double) = {
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

  def toDeps(sentence: Array[String]):ArrayBuffer[Dependency] = {
    val deps = new ArrayBuffer[Dependency]()
    for(line <- sentence) {
      // println(s"Converting line: $line")
      val tokens = line.split("\\s+")
      if(tokens.size < 8)
        throw new RuntimeException(s"ERROR: invalid output line: $line")
      val label = tokens(7)
      val head = tokens(6).toInt
      deps += new Dependency(label, head)
    }
    deps
  }

  def readDependencies(fn:String):Array[Dependency] = {
    val deps = new ArrayBuffer[Dependency]()
    for(line <- io.Source.fromFile(fn).getLines()) {
      val content = line.trim
      if(content.length > 0) {
        val tokens = content.split("\\s+")
        if(tokens.size < 8)
          throw new RuntimeException(s"ERROR: invalid output line in file $fn: $line")
        val label = tokens(7)
        val head = tokens(6).toInt
        deps += new Dependency(label, head)
      }
    }
    deps.toArray
  }

}

class Dependency(val label:String, val head:Int)
