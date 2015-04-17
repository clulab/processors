package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import java.io.BufferedInputStream
import java.util.zip.GZIPInputStream

import scala.io.Source

/**
  * Support methods for writing local KB accessors.
  *   Written by Tom Hicks. 4/16/2015.
  *   Last Modified: Refactor as methods in utility object.
  */
object LocalKBUtils {

  /** The set of characters to remove from the text to create a lookup key. */
  val KeyCharactersToRemove = " /-".toSet

  /** The set of words to remove from the text to create a lookup key. */
  // val KeyStopWords = Set("family", "protein")

  /** The set of words to remove from the text to create a lookup key. */
  val HumanLabels = Set("homo sapiens", "human")


  /** Tell whether the given species string is label for humans or not. */
  def isHumanSpecies (species: String): Boolean = {
    if (HumanLabels.contains(species.toLowerCase)) true else false
  }

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
