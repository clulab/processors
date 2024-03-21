package org.clulab.processors.hexatagging

import org.clulab.sequences.ColumnReader
import scala.collection.mutable.Stack

object DepsToTags {
  def main(args: Array[String]): Unit = {
    val depsFileName = "dynet/en/deps/universal/wsj/train.labels"
    val sents = ColumnReader.readColumns(depsFileName)
    println(s"Read ${sents.length} sentences.")

    for(sent <- sents) {
      val depTree = DependencyTree.toTree(sent)
      val stack = new Stack[BHT]
      depTree.toBHT(stack, "root")
      val bht = stack.pop()
      println(depTree)
      println(bht)
      System.exit(1)
    }
  }
}
