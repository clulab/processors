package org.clulab

import java.io.File

import org.clulab.learning.RVFDatum
import org.clulab.struct.Counter

import scala.io.Source

import org.clulab.processors.Document
import org.clulab.serialization.json.JSONSerializer
import org.json4s.jackson.JsonMethods._

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
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    val source = Source.fromInputStream(stream)
    val data = source.mkString
    source.close()
    data
  }

}
