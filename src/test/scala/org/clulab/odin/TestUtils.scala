package org.clulab.odin


object TestUtils {

  /**
    * Read contents of rule file, given some path
    * @param filename the path to a file
    * @return file contents as a String
    */
  def readFile(filename: String) = {
    val source = io.Source.fromFile(filename)
    val data = source.mkString
    source.close()
    data
  }
}
