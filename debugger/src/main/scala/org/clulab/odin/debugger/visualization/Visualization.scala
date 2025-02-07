package org.clulab.odin.debugger.visualization

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
