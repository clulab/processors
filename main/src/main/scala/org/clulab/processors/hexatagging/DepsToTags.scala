package org.clulab.processors.hexatagging

import org.clulab.sequences.ColumnReader

object DepsToTags {
  def main(args: Array[String]): Unit = {
    val depsFileName = "../dynet/en/deps/universal/wsj/train.labels"
    val sents = ColumnReader.readColumns(depsFileName)
    println(s"Read ${sents.length} sentences.")
  }
}
