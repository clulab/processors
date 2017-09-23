package org.clulab.processors.clu.bio

import java.io.{BufferedReader, InputStreamReader}
import java.util.regex.Pattern

import org.clulab.processors.clu.TokenizerPreProcessor
import org.clulab.processors.clu.bio.BioTokenizerPreProcessor._

import scala.collection.mutable

/**
  * Preprocesses bio text, including Unicode normalization, and removing figure and table references
  * User: mihais
  * Date: 9/10/17
  */
class BioTokenizerPreProcessor(removeFigTabReferences:Boolean, removeBibReferences:Boolean) extends TokenizerPreProcessor {
  val unicodes:Map[Char, String] = loadUnicodes

  override def process(origText: String): String = {
    val textWithoutUnicode = replaceUnicodeWithAscii(origText)
    val textWithoutBibRefs = removeBibRefs(textWithoutUnicode)
    val textWithoutFigRefs = removeFigureAndTableReferences(textWithoutBibRefs)
    textWithoutFigRefs
  }

  def replaceUnicodeWithAscii(origText:String):String = {
    val os = new StringBuilder()
    for(i <- 0 until origText.length) {
      val c: Char = origText.charAt(i)
      if(unicodes.contains(c)) {
        os.append(unicodes(c))
      } else {
        os.append(c)
      }
    }
    os.toString()
  }


  /**
    * Removes references to Bibliographies in the given text string, if the
    * removeBibReferences flag is set for this class instance.
    * @return The original text or the cleaned text, depending on setting of the
    *         removeBibReferences flag.
    */
  def removeBibRefs (text: String): String = {
    if (!removeBibReferences)               // remove bib refs only if flag is set
      return text

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
  def stringHasBibRef (stringInParens: String): Boolean = {
    stringInParens contains "XREF_BIBR"
  }


  def removeFigureAndTableReferences(origText:String):String = {
    if (!removeFigTabReferences) return origText

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

object BioTokenizerPreProcessor {
  /** Match a single text string bounded by non-nested parentheses. */
  val MATCHED_PARENS_NON_NESTED: Pattern = Pattern.compile("""\([^()]*\)""")

  val FIGTAB_REFERENCE_WITH_PARENS: Pattern = Pattern.compile("\\((\\s*see)?(\\s*supplementary)?\\s*(figure|table|fig\\.|tab\\.)[^\\)]*\\)", Pattern.CASE_INSENSITIVE)
  val FIGTAB_REFERENCE: Pattern = Pattern.compile("\\s*see(\\s*supplementary)?\\s*(figure|table|fig\\.|tab\\.)\\s*[0-9A-Za-z\\.]+", Pattern.CASE_INSENSITIVE)

  val UNICODE_TO_ASCII: String = "org/clulab/processors/bionlp/unicode_to_ascii.tsv"

  def loadUnicodes:Map[Char, String] = {
    val map = new mutable.HashMap[Char, String]()
    val is = getClass.getClassLoader.getResourceAsStream(UNICODE_TO_ASCII)
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
