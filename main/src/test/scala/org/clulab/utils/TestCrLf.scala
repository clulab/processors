package org.clulab.utils

import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader

import org.clulab.utils.Closer.AutoCloser

import org.scalatest._

class TestCrLf extends FlatSpec with Matchers {

  behavior of "resources"

  def test(file: File): Unit = {
    val path = file.getCanonicalPath
    val buffer = new Array[Char](1024)

    it should "not have any CrLf line endings in " + path in {
      val inputReader = new InputStreamReader(
        new BufferedInputStream(
          new FileInputStream(file)
        ),
        Sourcer.utf8
      )
      val hasCrLf = inputReader.autoClose { inputReader =>
        var hasCrLf = false
        var endedWithCr = false

        var readCount = inputReader.read(buffer)
        while (!hasCrLf && readCount > 0) {
          hasCrLf |= (endedWithCr && buffer(0) == '\n')
          hasCrLf |= buffer.containsSlice("\r\n")
          endedWithCr = buffer(readCount - 1) == '\r'
          readCount = inputReader.read(buffer)
        }
        hasCrLf
      }

      hasCrLf should be (false)
    }
  }

  // https://groups.google.com/forum/#!topic/scala-user/WrmYHHzcJPw
  type Operation = File => Unit

  /**
    * Lots of files have mixed line endings.  These four are consistently LF only.  They should
    * stay this way when the project is published.  If these remain unchanged, chances are that
    * the other ones do so as well.
    */
  val wantedSuffixes: Seq[String] = Seq("2PlanarEager.xml", "NivreEager.xml", "PlanarEager.xml", "StackAttardi.xml")
  val unwantedSuffixes: Seq[String] = Seq.empty

  def fileMatches(file: File): Boolean = {
    val canonicalPath = file.getCanonicalPath.replace('\\', '/')

    wantedSuffixes.exists(suffix => canonicalPath.endsWith(suffix)) &&
        !unwantedSuffixes.exists(suffix => canonicalPath.endsWith(suffix))
  }

  def directoryMatches(file: File): Boolean = true

  def doOperation(path: String)(operation: Operation): Unit = {
    for (files <- Option(new File(path).listFiles); file <- files) {
      if (file.isFile && fileMatches(file))
        operation(file)
      if (file.isDirectory && directoryMatches(file))
        doOperation(file.getAbsolutePath)(operation)
    }
  }

  /**
    * These files belong to a different project, one which has no test directory.
    * The files most likely to cause problems are located there, however.
    * Placement here is therefore a hack.  This code is mostly copied from Eidos.
    */
  doOperation(new File("./modelsmain/src/main/resources/appdata/features").getCanonicalPath)(test)
}
