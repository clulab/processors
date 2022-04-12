package org.clulab.dynet

import java.io.PrintWriter

trait Saveable {
  def saveX2i(printWriter: PrintWriter): Unit
}
