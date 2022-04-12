package org.clulab.utils

import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.{File, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets

import org.slf4j.{Logger, LoggerFactory}

class Sink(file: File, charsetName: String, append: Boolean = false) extends OutputStreamWriter(
  if (append) Sinker.newAppendingBufferedOutputStream(file) else Sinker.newBufferedOutputStream(file),
  charsetName)

object Sinker {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
  val utf8: String = StandardCharsets.UTF_8.toString

  def printWriterFromFile(file: File, append: Boolean): PrintWriter = {
    logger.info("Sinking file " + file.getPath)

    new PrintWriter(new Sink(file, Sourcer.utf8, append))
  }

  def printWriterFromFile(path: String, append: Boolean = false): PrintWriter = printWriterFromFile(new File(path), append)

  def newAppendingBufferedOutputStream(file: File): BufferedOutputStream =
    new BufferedOutputStream(new FileOutputStream(file, true))

  def newBufferedOutputStream(file: File): BufferedOutputStream =
    new BufferedOutputStream(new FileOutputStream(file))
}
