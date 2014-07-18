package edu.arizona.sista.utils

import java.io._
import java.nio.charset.Charset

/**
 * File utilities
 * User: mihais
 * Date: 1/9/14
 */
object Files {
  val TEMP_DIR_ATTEMPTS = 100

  def mkTmpDir(prefix:String, deleteOnExit:Boolean):String = {
    val baseDir = new File(System.getProperty("java.io.tmpdir"))

    // to minimize collisions, the dir name contains the time and the thread id
    val baseName = prefix + "-" + System.nanoTime().toString + "-" + Thread.currentThread().getId + "-"

    for(counter <- 0 until TEMP_DIR_ATTEMPTS) {
      val tempDir = new File(baseDir, baseName + counter.toString)
      if (tempDir.mkdir()) {
        if(deleteOnExit) tempDir.deleteOnExit()
        // println("work dir: " + tempDir.getAbsolutePath)
        return tempDir.getAbsolutePath
      }
    }

    throw new IllegalStateException("ERROR: Failed to create directory within "
      + TEMP_DIR_ATTEMPTS + " attempts (tried "
      + baseName + "0 to " + baseName + (TEMP_DIR_ATTEMPTS - 1) + ')');
  }

  val FILE_CHARSET = Charset.forName("ISO-8859-1")

  def toPrintWriter(w:Writer):PrintWriter = {
    w match {
      case pw:PrintWriter => pw
      case bw:BufferedWriter => new PrintWriter(bw)
      case _ => new PrintWriter(new BufferedWriter(w))
    }
  }

  def toBufferedReader(r:Reader):BufferedReader = {
    r match {
      case br:BufferedReader => br
      case _ => new BufferedReader(r)
    }
  }
}
