package org.clulab

import org.clulab.learning.RVFDatum
import org.clulab.processors.Document
import org.clulab.serialization.json.JSONSerializer
import org.clulab.struct.Counter
import org.json4s.jackson.JsonMethods._

import _root_.scala.io.Source
import _root_.scala.util.Using
import java.io.File

object TestUtils {

  def mkRVFDatum[L](label:L, features:List[String]):RVFDatum[L, String] = {
    val c = new Counter[String]
    for(f <- features) c.incrementCount(f)
    new RVFDatum[L, String](label, c)
  }

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
    Using.resource(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(path))) { source =>
      val data = source.mkString
      data
    }
  }
}
