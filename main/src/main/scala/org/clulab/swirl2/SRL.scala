package org.clulab.swirl2

import java.io._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

import org.clulab.processors.Sentence
import org.clulab.struct.{DirectedGraph, Edge}
import org.clulab.utils.Files
import org.clulab.utils.StringUtils._
import org.slf4j.LoggerFactory
import SRL._

/**
  * Implements the entire SRL pipeline
  * User: mihais
  * Date: 3/3/16
  * Last Modified: Fix compiler issue: import scala.io.Source.
  */
class SRL {
  var predClassifier:Option[PredicateClassifier] = None
  var argClassifier:Option[ArgumentClassifier] = None

  def saveTo(w:Writer): Unit = {
    val pw = Files.toPrintWriter(w)

    //
    // liblinear closes the stream when saving its model
    // so we need to save each model separately and then transfer them to the main writer
    //

    val predFile = File.createTempFile("tmp_pc", ".dat")
    logger.debug(s"Saving the predicate classifier to ${predFile.getAbsolutePath}...")
    val predWriter = new PrintWriter(new BufferedWriter(new FileWriter(predFile)))
    predClassifier.foreach(_.saveTo(predWriter))
    predWriter.close()
    transferContent(predFile, pw)
    predFile.delete()

    val argFile = File.createTempFile("tmp_ac", ".dat")
    logger.debug(s"Saving the argument classifier to ${argFile.getAbsolutePath}...")
    val argWriter = new PrintWriter(new BufferedWriter(new FileWriter(argFile)))
    argClassifier.foreach(_.saveTo(argWriter))
    argWriter.close()
    transferContent(argFile, pw)
    argFile.delete()

  }

  def transferContent(fromFile:File, to:PrintWriter): Unit = {
    val source = Source.fromFile(fromFile)
    for(line <- source.getLines())
      to.println(line)
    source.close()
  }

  def train(trainPath:String): Unit = {
    logger.info("Training the predicate classifier...")
    predClassifier = Some(new PredicateClassifier)
    predClassifier.get.train(trainPath)
    logger.info("Completed training the predicate classifier.")

    logger.info("Training the argument classifier...")
    argClassifier = Some(new ArgumentClassifier)
    argClassifier.get.train(trainPath)
    logger.info("Completed training the argument classifier.")

    // TODO: add argument labeler
  }

  def test(testPath:String):Unit = {
    val reader = new Reader
    val doc = reader.load(testPath)
    val output = new ListBuffer[(DirectedGraph[String], DirectedGraph[String])] // gold, predicted

    for(s <- doc.sentences) {
      val frames = classifySentence(s)

      //println(s.words.mkString(" "))
      //println(frames)

      output += new Tuple2(s.semanticRoles.get, frames)
    }

    score(output.toList)
  }

  def classifySentence(sentence:Sentence):DirectedGraph[String] = {
    val roots = new mutable.HashSet[Int]()
    val edges = new ListBuffer[Edge[String]]

    // first, find predicates
    /*
    for(i <- sentence.words.indices) {
      val scores = predClassifier.get.classify(sentence, i)
      val isPredicate = scores.getCount(PredicateClassifier.POS_LABEL) >= PredicateClassifier.POS_THRESHOLD
      if(isPredicate) {
        roots += i
      }
    }
    */
    for(i <- sentence.words.indices) {
      if(argClassifier.get.isPred(i, sentence)) {
        roots += i
      }
    }

    // second, find arguments for each predicate
    for(pred <- roots) {
      val history = new ArrayBuffer[(Int, String)]()
      for(arg <- sentence.words.indices) {
        var predLabel = ArgumentClassifier.NEG_LABEL
        if (ValidCandidate.isValid(sentence, arg, pred)) {
          val scores = argClassifier.get.classify(sentence, arg, pred, history).sorted
          predLabel = scores.head._1
          if (predLabel != ArgumentClassifier.NEG_LABEL) {
            history += new Tuple2(arg, predLabel)
            edges += Edge[String](source = pred , destination = arg, predLabel)
          }
        }
      }
    }

    DirectedGraph[String](edges.toList, roots.toSet)
  }

  def score(output:List[(DirectedGraph[String], DirectedGraph[String])]): Unit = {
    var total = 0
    var predicted = 0
    var correctUnlabeled = 0

    for(o <- output) {
      val gold = o._1.allEdges
      val pred = o._2.allEdges

      total += gold.size
      predicted += pred.size

      for(p <- pred) {
        var found = false
        for(g <- gold if ! found) {
          if(p._1 == g._1 &&
             p._2 == g._2) {
            correctUnlabeled += 1
            found = true
          }
        }
      }
    }

    val p = correctUnlabeled.toDouble / predicted
    val r = correctUnlabeled.toDouble / total
    val f1 = 2.0 * p * r / (p + r)

    println(s"Unlabeled precision: $p ($correctUnlabeled/$predicted)")
    println(s"Unlabeled recall: $r ($correctUnlabeled/$total)")
    println(s"Unlabeled F1: $f1")
  }
}

object SRL {
  val logger = LoggerFactory.getLogger(classOf[SRL])

  def main(args:Array[String]) {
    val props = argsToProperties(args)
    var srl = new SRL

    if (props.containsKey("train")) {
      srl.train(props.getProperty("train"))

      if (props.containsKey("model")) {
        val os = new PrintWriter(new BufferedWriter(new FileWriter(props.getProperty("model"))))
        srl.saveTo(os)
        os.close()
      }
    }

    if (props.containsKey("test")) {
      if (props.containsKey("model")) {
        val is = new BufferedReader(new FileReader(props.getProperty("model")))
        srl = loadFrom(is)
        is.close()
      }

      srl.test(props.getProperty("test"))
    }
  }

  def loadFrom(r:java.io.Reader):SRL = {
    val srl = new SRL
    val reader = Files.toBufferedReader(r)

    val pc = PredicateClassifier.loadFrom(reader)
    val ac = ArgumentClassifier.loadFrom(reader)
    srl.predClassifier = Some(pc)
    srl.argClassifier = Some(ac)

    srl
  }
}
