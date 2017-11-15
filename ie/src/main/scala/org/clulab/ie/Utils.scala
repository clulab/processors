package org.clulab.ie

import java.io.InputStream


object Utils {

  // methods for reading rules

  def streamFromResource(path: String): InputStream = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    stream
  }

  def readResource(path: String): String = {
    val stream = streamFromResource(path)
    val source = scala.io.Source.fromInputStream(stream)
    val data = source.mkString
    source.close()
    data
  }
}
