package edu.arizona.sista.processors.bionlp

import java.io.{InputStreamReader, BufferedReader}
import java.util.regex.Pattern

import scala.StringBuilder
import scala.collection.mutable

/**
 * Preprocesses bio text, including Unicode normalization, and removing figure and table references
 * User: mihais
 * Date: 8/14/15
 */
class BioNLPPreProcessor(removeFigTabReferences:Boolean) {
  val unicodes = BioNLPPreProcessor.loadUnicodes

  def preprocess(origText:String):String = {
    val textWithoutUnicode = replaceUnicodeWithAscii(origText)
    val textWithoutFigRefs = removeFigureAndTableReferences(textWithoutUnicode)
    textWithoutFigRefs
  }

  def replaceUnicodeWithAscii(origText:String):String = {
    val os = new StringBuilder()
    for(i <- 0 until origText.length) {
      val c = origText.charAt(i)
      if(unicodes.contains(c)) {
        os.append(unicodes.get(c).get)
      } else {
        os.append(c)
      }
    }
    os.toString()
  }

  def removeFigureAndTableReferences(origText:String):String = {
    if (!removeFigTabReferences) return origText

    var noRefs = origText
    // the pattern with parens must run first!
    noRefs = removeFigTabRefs(BioNLPPreProcessor.FIGTAB_REFERENCE_WITH_PARENS, noRefs)
    noRefs = removeFigTabRefs(BioNLPPreProcessor.FIGTAB_REFERENCE, noRefs)
    noRefs
  }

  /**
   * Removes references to Tables and Figures
   * @param pattern Fig/Tab pattern
   * @param text The original text
   * @return The cleaned text
   */
  def removeFigTabRefs(pattern:Pattern, text:String):String = {
    val m = pattern.matcher(text)
    val b = new StringBuilder
    var previousEnd = 0
    while(m.find()) {
      b.append(text.substring(previousEnd, m.start()))
      // white out the reference, keeping the same number of characters
      for(i <- m.start() until m.end()) b.append(" ")
      previousEnd = m.end()
    }
    if(previousEnd < text.length)
      b.append(text.substring(previousEnd))
    b.toString()
  }
}

object BioNLPPreProcessor {
  val FIGTAB_REFERENCE_WITH_PARENS = Pattern.compile("\\((\\s*see)?(\\s*supplementary)?\\s*(figure|table|fig\\.|tab\\.)[^\\)]*\\)", Pattern.CASE_INSENSITIVE)
  val FIGTAB_REFERENCE = Pattern.compile("\\s*see(\\s*supplementary)?\\s*(figure|table|fig\\.|tab\\.)\\s*[0-9A-Za-z\\.]+", Pattern.CASE_INSENSITIVE)

  val UNICODE_TO_ASCII = "edu/arizona/sista/processors/bionlp/unicode_to_ascii.tsv"

  def loadUnicodes:Map[Char, String] = {
    val map = new mutable.HashMap[Char, String]()
    val is = BioNLPPreProcessor.getClass.getClassLoader.getResourceAsStream(UNICODE_TO_ASCII)
    assert(is != null, s"Failed to find resource file $UNICODE_TO_ASCII in the classpath!")
    val reader = new BufferedReader(new InputStreamReader(is))
    var done = false
    while(! done) {
      var line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        line = line.trim
        if(! line.startsWith("#")) {
          val tokens = line.split("\\t")
          if(tokens.length > 2)
            throw new RuntimeException(s"ERROR: invalid line [$line] in resource file $UNICODE_TO_ASCII")
          if(tokens.length == 1) {
            map += (toUnicodeChar(tokens(0)) -> "")
          } else {
            map += (toUnicodeChar(tokens(0)) -> tokens(1))
          }
        }
      }
    }
    reader.close()
    map.toMap
  }

  def toUnicodeChar(s:String):Char = {
    Integer.parseInt(s, 16).toChar
  }
}
