package org.clulab.odin.debugger.visualizer

import org.clulab.odin.impl.Extractor

import java.io.{PrintWriter, StringWriter}
import scala.util.Using

abstract class Visualizer() {
  def visualize(extractor: Extractor): Visualization = ???

  def printToString(f: PrintWriter => Unit): String = {
    val stringWriter = new StringWriter()

    Using.resource(new PrintWriter(stringWriter))  { printWriter =>
      f(printWriter)
    }

    stringWriter.toString
  }
}
