package edu.arizona.sista.processors.bionlp

import java.util.regex.Pattern

/**
 * Preprocesses bio text, including Unicode normalization, and removing figure and table references
 * User: mihais
 * Date: 8/14/15
 */
class BioNLPPreProcessor(removeFigTabReferences:Boolean) {
  def preprocess(origText:String):String = {
    val textWithoutUnicode = replaceUnicodeWithAscii(origText)
    val textWithoutFigRefs = removeFigureAndTableReferences(textWithoutUnicode)
    textWithoutFigRefs
  }

  def replaceUnicodeWithAscii(origText:String):String = {
    origText
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

}
