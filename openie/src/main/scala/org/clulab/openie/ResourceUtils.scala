package org.clulab.openie

import org.clulab.scala.Using._

import java.io.InputStream

object ResourceUtils {

  // methods for reading rules

  def streamFromResource(path: String): InputStream = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    stream
  }

  def readResource(path: String): String = {
    Using.resource(scala.io.Source.fromInputStream(streamFromResource(path))) { source =>
      source.mkString
    }
  }
}
