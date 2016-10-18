package org.clulab.odin

import java.io.File
import org.clulab.processors.Document
import org.json4s.native.JsonMethods._
import org.clulab.serialization.json._


object TestUtils {

  /**
    * Read contents of rule file in the classpath, given some path
    *
    * @param path the path to a file
    * @return file contents as a String
    */
  def readFile(path: String) = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    val source = io.Source.fromInputStream(stream)
    val data = source.mkString
    source.close()
    data
  }

}
