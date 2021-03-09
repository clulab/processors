package org.clulab.utils

import java.io.FileInputStream
import java.io.InputStream

import scala.util.Failure
import scala.util.Try

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
  }

  protected def getInputStream(name: String, fileLocation: String, resourceLocation: String): Option[StreamResult] = {
    val binName = name + InputStreamer.binExtension
    val txtName = name + InputStreamer.txtExtension
    val streamResult = Failure(null)
        .orElse(Try(StreamResult(getFileAsStream(fileLocation + binName),         Location.File,     Format.Bin)))
        .orElse(Try(StreamResult(getFileAsStream(fileLocation + txtName),         Location.File,     Format.Txt)))
        .orElse(Try(StreamResult(getResourceAsStream(resourceLocation + binName), Location.Resource, Format.Bin)))
        .orElse(Try(StreamResult(getResourceAsStream(resourceLocation + txtName), Location.Resource, Format.Txt)))
        .toOption

    streamResult
  }

  def stream(name: String, fileLocation: String, resourceLocation: String): Option[StreamResult] =
    getInputStream(name, fileLocation, resourceLocation)

  def stream(name: String, location: String): Option[StreamResult] =
    stream(name, location, location)

  def stream(path: String): Option[StreamResult] = {
    val name = StringUtils.afterLast(path, '/', all = true, keep = false)
    val location = StringUtils.beforeLast(path, '/', all = false, keep = true)

    stream(name, location)
  }
}

object InputStreamer {
  val binExtension = ".bin"
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
