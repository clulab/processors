package org.clulab.utils

import sun.net.www.protocol.jar.JarURLConnection

import java.io.FileInputStream
import java.io.InputStream
import scala.util.Failure
import scala.util.Try

class PublicCloseInputStream(inputStream: InputStream) extends InputStream {

  override def read(): Int = inputStream.read()

  // This can be reflected upon.
  override def close(): Unit = inputStream.close()
}

class InputStreamer(val provider: AnyRef = InputStreamer, direct: Boolean = true) {
  import InputStreamer.Format
  import InputStreamer.StreamResult
  import InputStreamer.Location

  def getFileAsStream(name: String): FileInputStream = new FileInputStream(name)

  def getResourceAsStream(name: String): InputStream = {
    val inputStream =
      if (direct) provider.getClass.getResourceAsStream(name)
      else provider.getClass.getClassLoader.getResourceAsStream(name)

    Option(inputStream).getOrElse(throw new RuntimeException(s"Resource $name not found."))
    // The inputStream may be a JarURLConnection#JarURLInputStream which is Closeable but
    // whose close method is not discoverable using reflection at runtime so that an AutoClose
    // cannot be constructed without JVM options like --add-opens which we want to avoid.
    // The exception is this:
    // Cause: java.lang.reflect.InaccessibleObjectException: Unable to make public void
    // sun.net.www.protocol.jar.JarURLConnection$JarURLInputStream.close() throws
    // java.io.IOException accessible: module java.base does not "opens sun.net.www.protocol.jar"
    // to unnamed module @5ffead27.
    // Update: Use of PublicCloseInputStream was found the be incredibly slow, so the use of
    // autoClose in WordEmbeddingMapPool was replaced by a try and finally on the raw inputStream.
    // new PublicCloseInputStream(inputStream)
  }

  protected def getInputStream(name: String, fileLocation: String, resourceLocation: String,
      txtExtension: String, binExtension: String): Option[StreamResult] = {
    val binName = name + binExtension
    val txtName = name + txtExtension
    val streamResult = Failure(null)
        .orElse(Try(StreamResult(getFileAsStream(fileLocation + binName),         Location.File,     Format.Bin)))
        .orElse(Try(StreamResult(getFileAsStream(fileLocation + txtName),         Location.File,     Format.Txt)))
        .orElse(Try(StreamResult(getResourceAsStream(resourceLocation + binName), Location.Resource, Format.Bin)))
        .orElse(Try(StreamResult(getResourceAsStream(resourceLocation + txtName), Location.Resource, Format.Txt)))
        .toOption

    streamResult
  }

  def stream(name: String, fileLocation: String, resourceLocation: String,
      txtExtension: String = InputStreamer.txtExtension, binExtension: String = InputStreamer.binExtension):
      Option[StreamResult] =
    getInputStream(name, fileLocation, resourceLocation, txtExtension, binExtension)

  def stream(name: String, location: String): Option[StreamResult] =
    stream(name, location, location)

  def stream(path: String): Option[StreamResult] = {
    val name = StringUtils.afterLast(path, '/', all = true, keep = false)
    val location = StringUtils.beforeLast(path, '/', all = false, keep = true)

    stream(name, location)
  }
}

object InputStreamer {
  /**
    * The terms bin and txt here are slight misnomers.  Perhaps they should be computer readable
    * and human readable, compressed and expanded, digested and raw, optimized and inefficient.
    * The typical example is that some .txt file requiring much parsing and error checking is
    * ingested once and then saved in a .bin file that can be read with little interpretation.
    * These are the default extensions that can be changed in the call to stream().
    */
  val binExtension = ".kryo"
  val txtExtension = ".txt"

  object Location extends Enumeration {
    type Location = Value
    val File, Resource = Value
  }

  object Format extends Enumeration {
    type Format = Value
    val Txt, Bin = Value
  }

  case class StreamResult(inputStream: InputStream, location: Location.Location, format: Format.Format)
}
