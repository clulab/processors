package org.clulab

import java.io.File

import org.clulab.processors.Document
import org.json4s.jackson.JsonMethods._
import org.clulab.serialization.json.JSONSerializer


object TestUtils {

  def jsonStringToDocument(jsonstr: String): Document = JSONSerializer.toDocument(parse(jsonstr))

  def readResourceAsFile(path: String): File = {
    val url = getClass.getClassLoader.getResource(path)
    new File(url.toURI)
  }

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
