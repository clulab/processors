package edu.arizona.sista.discourse.rstparser

import java.io.File
import scala.collection.mutable.ListBuffer
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
 * Reads all .dis files in a directory and computes some useful stats
 * User: mihais
 * Date: 4/6/14
 */
object TreeStats {
  def main(args:Array[String]) {
    val reader = new Reader
    lazy val proc = new FastNLPProcessor()
    val top = new File(args(0))
    val trees = new ListBuffer[DiscourseTree]
    if(top.isDirectory) {
      for(f <- top.listFiles()){
        if(f.getName.endsWith(".dis")){
          println("Parsing file " + f)
          val p = reader.read(f, proc, simplifyRelationLabels=true, verbose=true)
          trees += p._1
          //println(p)
        }
      }
    } else {
      val p = reader.read(top, proc, verbose=true)
      trees += p._1
      //println(p)
    }

    labelStats(trees.toList)
    countNodesWithMoreThanTwoChildren(trees.toList)
  }

  def countNodesWithMoreThanTwoChildren(trees:List[DiscourseTree]) {
    val stats = new Counter[String]()
    for(t <- trees) countNodesWithMoreThanTwoChildren(t, stats)
    println("Distribution of labels with more than two children:")
    for(s <- stats.sorted) {
      println("\t" + s._1 + " " + s._2)
    }
  }

  def countNodesWithMoreThanTwoChildren(t:DiscourseTree, stats:Counter[String]) {
    if(t.children != null) {
      if(t.children.length > 2)
        stats.incrementCount(t.relationLabel + " (" + t.relationDirection + ")")
      for(c <- t.children)
        countNodesWithMoreThanTwoChildren(c, stats)
    }
  }

  def labelStats(trees:List[DiscourseTree]) {
    val stats = new Counter[String]()
    for(t <- trees) labelStats(t, stats)
    println("Distribution of labels:")
    for(s <- stats.sorted) {
      println("\t" + s._1 + " " + s._2)
    }
  }

  def labelStats(t:DiscourseTree, stats:Counter[String]) {
    if(! t.isTerminal) {
      stats.incrementCount(t.relationLabel, 1)
      for(c <- t.children)
        labelStats(c, stats)
    }
  }
}
