package org.clulab.processors.clu.sequences

import scala.collection.mutable.ArrayBuffer

trait FirstPassLabelsReader {
  protected def readFirstPassLabels(source:scala.io.Source):Array[Array[String]] = {
    val lines = new ArrayBuffer[Array[String]]()
    for(line <- source.getLines()) {
      lines += line.split("\\t")
    }
    lines.toArray
  }
}
