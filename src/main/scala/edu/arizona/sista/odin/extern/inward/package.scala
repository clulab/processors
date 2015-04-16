package edu.arizona.sista.odin.extern

import java.io.BufferedInputStream
import java.util.zip.GZIPInputStream

import scala.io.Source

package object inward {

  def sourceFromResource (resourcePath:String): Source = {
    val inStream = this.getClass.getResourceAsStream(resourcePath)
    if (resourcePath.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(inStream)))
    else
      Source.fromInputStream(inStream)
  }

}
