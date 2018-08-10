package org.clulab.utils

import java.io.{BufferedReader, InputStreamReader}
import java.util.regex.Pattern
import java.text.Normalizer

import org.clulab.utils.ScienceUtils._

import scala.collection.mutable

class ScienceUtils {
  val unicodes:Map[Char, String] = loadUnicodes
  val accents:Set[Char] = loadAccents

  /**
    * Replaces common Unicode characters with the corresponding ASCII string, e.g., \u0277 is replaced with "omega"
    * @param origText The text to be processed
    * @return The processed text
    */
  def replaceUnicodeWithAscii(origText:String, keepAccents:Boolean = false):String = {
    val normText = normalizeUnicode(origText)
    val os = new StringBuilder()
    for(i <- 0 until normText.length) {
      val c: Char = normText.charAt(i)
      if(unicodes.contains(c)) {
        if(keepAccents && accents.contains(c))
          os.append(c)
        else
          os.append(unicodes(c))
      } else {
        os.append(c)
      }
    }
    os.toString()
  }

  /**
    * Replaces unknown Unicode characters with space characters.
    * This uses the unicodes variable for the list of known substitutions.
    * This method may be coupled with the above replaceUnicodeWithAscii()
    * for text cleaning or one can do both with replaceUnicode() below.
    */
  def replaceUnknownUnicodeWithSpaces(origText: String): String = {
    val stringBuilder = new StringBuilder()
    origText.foreach(c => stringBuilder.append(if (unicodes.contains(c) || c < 0x80) c else ' '))
    stringBuilder.toString()
  }

  /**
    * Replaces all Unicode characters (characters with codePoints >= 80) with either a
    * substitute ASCII string from a known list of substitutions or simply a space.
    * It leaves other characters alone (given certain assumptions about unicode).
    */
  def replaceUnicode(origText: String): String = {
    val stringBuilder = new StringBuilder()
    origText.foreach { c =>
      if (unicodes.contains(c)) stringBuilder.append(unicodes(c)) // Append a String.
      else stringBuilder.append(if (c < 0x80) c else ' ') // Append a Char.
    }
    stringBuilder.toString()
  }


  /**
    * Replaces references to bibliographies in the given text string with white spaces. The number of characters is preserved.
    * @param text The text to be processed
    * @return The processed text
    */
  def removeBibRefs (text: String): String = {
    val m = MATCHED_PARENS_NON_NESTED.matcher(text)
    val bldr = new StringBuilder
    var previousEnd = 0
    while (m.find()) {                      // while finding sets of matching parentheses
      bldr.append(text.substring(previousEnd, m.start())) // save text up to match

      val inParens = text.substring(m.start(), m.end()) // get text in parens
      if (stringHasBibRef(inParens))        // if text contained a bib reference
        bldr.append(" " * inParens.length)  // white out the reference w/ same number of chars
      else                                  // else concatenate text in parentheses
        bldr.append(inParens)
      previousEnd = m.end()                 // move the end pointer
    }
    if (previousEnd < text.length)          // add any leftover text
      bldr.append(text.substring(previousEnd))
    bldr.toString()
  }

  /** Tell whether the given parentheses-bounded string contains a bibliographic reference or not. */
  private def stringHasBibRef (stringInParens: String): Boolean = {
    stringInParens contains "XREF_BIBR"
  }

  /**
    * Replaces figure and table references from a given string with white spaces. The number of characters is preserved.
    * @param origText The text to be processed
    * @return The processed text
    */
  def removeFigureAndTableReferences(origText:String):String = {
    var noRefs = origText
    // the pattern with parens must run first!
    noRefs = removeFigTabRefs(FIGTAB_REFERENCE_WITH_PARENS, noRefs)
    noRefs = removeFigTabRefs(FIGTAB_REFERENCE, noRefs)
    noRefs
  }

  /**
    * Removes references to Tables and Figures
    * @param pattern Fig/Tab pattern
    * @param text The original text
    * @return The cleaned text
    */
  private def removeFigTabRefs(pattern:Pattern, text:String):String = {
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

/**
  * Implements several common utilities we need in the science domain
  */
object ScienceUtils {
  /** Match a single text string bounded by non-nested parentheses. */
  val MATCHED_PARENS_NON_NESTED: Pattern = Pattern.compile("""\([^()]*\)""")

  val FIGTAB_REFERENCE_WITH_PARENS: Pattern = Pattern.compile("\\((\\s*see)?(\\s*supplementary)?\\s*(figure|table|fig\\.|tab\\.)[^\\)]*\\)", Pattern.CASE_INSENSITIVE)
  val FIGTAB_REFERENCE: Pattern = Pattern.compile("\\s*see(\\s*supplementary)?\\s*(figure|table|fig\\.|tab\\.)\\s*[0-9A-Za-z\\.]+", Pattern.CASE_INSENSITIVE)

  val UNICODE_TO_ASCII: String = "org/clulab/processors/bionlp/unicode_to_ascii.tsv"
  val ACCENTED_CHARACTERS: String = "org/clulab/processors/bionlp/accented_characters.tsv"

  private def loadAccents:Set[Char] = {
    val acf = getClass.getClassLoader.getResourceAsStream(ACCENTED_CHARACTERS)
    assert(acf != null, s"Failed to find resource file $ACCENTED_CHARACTERS in the classpath!")
    val reader = new BufferedReader(new InputStreamReader(acf))
    val accents = new mutable.ArrayBuffer[Char]()
    var done = false
    while(! done) {
      val line = normalizeUnicode(reader.readLine())
      if(line == null) {
        done = true
      } else if (line.trim().nonEmpty) {
        accents.append(line.charAt(0))
      }
    }
    accents.toSet
  }

  private def loadUnicodes:Map[Char, String] = {
    val map = new mutable.HashMap[Char, String]()
    val is = getClass.getClassLoader.getResourceAsStream(UNICODE_TO_ASCII)
    assert(is != null, s"Failed to find resource file $UNICODE_TO_ASCII in the classpath!")
    val reader = new BufferedReader(new InputStreamReader(is))
    var done = false
    while(! done) {
      var line = normalizeUnicode(reader.readLine())
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

  def normalizeUnicode(text: String): String = {
    if (text == null) {
      null
    } else {
      Normalizer.normalize(text, Normalizer.Form.NFKC)
    }
  }

  private def toUnicodeChar(s:String):Char = {
    Integer.parseInt(s, 16).toChar
  }
}
