package org.clulab.processors.hexatagging

import org.clulab.sequences.ColumnReader
import scala.collection.mutable.Stack
import org.clulab.sequences.Row

object DepsToTags {
  def generateHexaTags(sent: Array[Row]): Unit = {
    val depTree = DependencyTree.toTree(sent)
    val stack = new Stack[BHT]
    depTree.toBHT(stack, "root")
    val bht = stack.pop()
    val (termTags, nonTermTags) = assignHexaTags(bht, sent.length)
    
    println(depTree)
    println(bht)
    println("termTags = " + termTags.mkString(", "))
    println("nonTermTags = " + nonTermTags.mkString(", "))
  }

  def assignHexaTags(bht: BHT, length: Int): (Array[String], Array[String]) = {
    val termTags = new Array[String](length)
    val nonTermTags = new Array[String](length)
    nonTermTags(length - 1) = "eos"

    // TODO!

    (termTags, nonTermTags)
  }

  def main(args: Array[String]): Unit = {
    val depsFileName = "dynet/en/deps/universal/wsj/train.labels"
    val sents = ColumnReader.readColumns(depsFileName)
    println(s"Read ${sents.length} sentences.")

    for(sent <- sents) {
      generateHexaTags(sent)
      System.exit(1)
    }
  }
}
