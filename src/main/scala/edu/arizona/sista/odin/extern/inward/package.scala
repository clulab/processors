package edu.arizona.sista.odin.extern

import java.io.BufferedInputStream
import java.util.zip.GZIPInputStream

import scala.io.Source

/**
  * Support methods for the inward package.
  *   Written by Tom Hicks. 4/16/2015.
  *   Last Modified: Add doc strings, method to make storage and lookup keys.
  */
package object inward {

  /** The set of characters to remove from the text to create a lookup key. */
  val KeyCharactersToRemove = " /-".toSet

  /** The set of words to remove from the text to create a lookup key. */
  // val KeyStopWords = Set("family", "protein")


  /** Canonicalize the given text string into a key for storage and lookup. */
  def makeKBCanonKey (text:String): String = {
    val key:String = text.toLowerCase
    // KeyStopWords.foreach { word => key = key.replaceAll(word, "") }
    key.filterNot(KeyCharactersToRemove)
  }


  /** Return a Scala Source object created from the given resource path string. If the
    * resource path ends with ".gz" the source is created around a gzip input stream. */
  def sourceFromResource (resourcePath:String): Source = {
    val inStream = this.getClass.getResourceAsStream(resourcePath)
    if (resourcePath.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(inStream)))
    else
      Source.fromInputStream(inStream)
  }

}
