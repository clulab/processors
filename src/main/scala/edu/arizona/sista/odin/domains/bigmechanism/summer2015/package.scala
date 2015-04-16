package edu.arizona.sista.odin.domains.bigmechanism

import java.io.BufferedInputStream
import java.util.zip.GZIPInputStream

import scala.io.Source

package object summer2015 {

  def streamFromResource (resourcePath:String): Source = {
    val inStream = this.getClass.getResourceAsStream(resourcePath)
    if (resourcePath.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(inStream)))
    else
      Source.fromInputStream(inStream)
  }

}
