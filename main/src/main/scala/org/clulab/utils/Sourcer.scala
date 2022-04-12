package org.clulab.utils

import java.io.{File, FileNotFoundException}
import java.nio.charset.StandardCharsets

import org.slf4j.LoggerFactory

import scala.io.BufferedSource
import scala.io.Source

object Sourcer {
  val logger = LoggerFactory.getLogger(this.getClass())
  val utf8 = StandardCharsets.UTF_8.toString
  
  def sourceFromResource(path: String, encoding: String = utf8): BufferedSource = {
    val url = Option(Sourcer.getClass.getResource(path))
        .getOrElse(throw newFileNotFoundException(path))

    logger.info("Sourcing resource " + url.getPath())
    Source.fromURL(url, encoding)
  }
  
  def sourceFromFile(file: File, encoding: String = utf8): BufferedSource = {
    logger.info("Sourcing file " + file.getPath())
    Source.fromFile(file, encoding)
  }

  def sourceFromFilename(filename: String, encoding: String = utf8): BufferedSource = sourceFromFile(new File(filename), encoding)

  def newFileNotFoundException(path: String): FileNotFoundException = {
    val message1 = path + " (The system cannot find the path specified"
    val message2 = message1 + (if (path.startsWith("~")) ".  Make sure to not use the tilde (~) character in paths in lieu of the home directory." else "")
    val message3 = message2 + ")"

    new FileNotFoundException(message3)
  }
}
