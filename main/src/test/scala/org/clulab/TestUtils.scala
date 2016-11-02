package org.clulab

import java.io.File
import org.clulab.processors.Document
import org.json4s.native.JsonMethods._
import org.clulab.serialization.json._


object TestUtils {
  def jsonStringToDocument(jsonstr: String): Document = JSONSerializer.toDocument(parse(jsonstr))
  def readResourceAsFile(path: String): File = {
    val url = getClass.getClassLoader.getResource(path)
    new File(url.toURI)
  }
}
