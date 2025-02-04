package org.clulab.odin.debugger.visualizer

import org.clulab.utils.FileUtils

import java.io.File
import scala.util.Using

trait Visualization {
  def toString: String

  def toFile(file: File): Unit = {
    Using.resource(FileUtils.printWriterFromFile(file)) { printWriter =>
      printWriter.println(toString)
    }
  }

  def toScreen(): Unit = println(toString)
}

class TextVisualization(text: String) extends Visualization {

  override def toString: String = text
}
