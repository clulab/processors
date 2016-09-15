package org.clulab.odin


object TestUtils {

  /**
    * Read contents of rule file in the classpath, given some path
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
