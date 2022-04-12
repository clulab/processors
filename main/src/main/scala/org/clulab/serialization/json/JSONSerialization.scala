package org.clulab.serialization.json

import java.io.File
import java.nio.charset.StandardCharsets
import org.apache.commons.io.FileUtils
import org.json4s._


trait JSONSerialization {

  def jsonAST: JValue

  def json(pretty: Boolean = false): String = stringify(jsonAST, pretty)

  /**
    * Serialize json file
    */
  def saveJSON(file: String, pretty: Boolean): Unit = {
    //require(file.endsWith(".json"), "file should have .json extension")
    val outFile = new File(file)
    FileUtils.writeByteArrayToFile(outFile, this.json(pretty).getBytes(StandardCharsets.UTF_8))
  }
  def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)

}