package org.clulab.processors.clu.bio

import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer
import BioTokenizerPostProcessor._
import org.clulab.struct.MutableNumber

/**
  * Processes tokenization so it suits bio analysis
  *
  * @param tokensWithValidSlash Some tokens may contain slashes (e.g., protein families); do not tokenize these
  * User: mihais
  * Date: 9/11/17
  */
class BioTokenizerPostProcessor(val tokensWithValidSlash:Set[String]) {
  /**
    * Implements the bio-specific post-processing steps from McClosky et al. (2011)
    * @param input  Input CoreNLP sentence
    * @return  The modified tokens
    */
  def process(input:Array[PostProcessorToken]):Array[PostProcessorToken] = {
    var tokens = input

    // revert tokenization that is too aggressive
    tokens = revertAggressiveTokenization(tokens)

    // "non" is a special prefix, because it drives negation detection
    tokens = breakOnPattern(tokens, Pattern.compile("(non)(-)(\\w+)", Pattern.CASE_INSENSITIVE))

    // tokenize around "-" when the suffix is a known verb, noun, or other important word
    tokens = breakOnPattern(tokens, dashSuffixes)

    // tokenize around "-" when the prefix is a known site, such as "Tyr" in "Tyr-phosphorylated"
    tokens = breakOnPattern(tokens, sitePrefixes)

    // break n-ary complexes
    tokens = breakNaryComplex(tokens)

    // break mutations
    // TODO: this needs improvement, see Dane's comments
    tokens = breakMutant(tokens, SINGLEDASH_PATTERN)

    // break all (well, most) tokens containing a single slash; try to replace them with an enumeration
    // does not apply to protein family names, which often contain slashes
    tokens = breakOneSlash(tokens, SINGLESLASH_PATTERN)

    // re-join trailing or preceding - or + to previous digit
    tokens = joinSigns(tokens)

    tokens
  }

  def isSpecialToken(s:String):Boolean = tokensWithValidSlash.contains(s.toLowerCase)

  def breakOnPattern(tokens:Array[PostProcessorToken], pattern:Pattern):Array[PostProcessorToken] = {
    val output = new ArrayBuffer[PostProcessorToken]
    for(i <- tokens.indices) {
      val token = tokens(i)
      val matcher = pattern.matcher(token.word)
      if (matcher.matches()) {
        val sepPos = matcher.start(2)
        val s1 = token.word.substring(0, sepPos)
        if(COMMON_PREFIXES.contains(s1.toLowerCase)) {
          // do not separate here; these prefixes cannot live on their own
          output += token
        } else {
          if (!DISCARD_STANDALONE_DASHES || !s1.equals("-")) {
            output += PostProcessorToken(s1, token.beginPosition, sepPos)
          }
          val sep = matcher.group(2)
          if (!DISCARD_STANDALONE_DASHES || !sep.equals("-")) {
            output += PostProcessorToken(sep, token.beginPosition + sepPos, 1)
          }
          val s3 = token.word.substring(sepPos + 1)
          if (!DISCARD_STANDALONE_DASHES || !s3.equals("-")) {
            output += PostProcessorToken(s3, token.beginPosition + sepPos + 1,
              token.endPosition - token.beginPosition - sepPos - 1)
          }
        }
      } else {
        output += token
      }
    }
    output.toArray
  }
}

object BioTokenizerPostProcessor {
  private val DISCARD_STANDALONE_DASHES = true

  private val VALID_DASH_SUFFIXES = Set(
    "\\w+ed", "\\w+ing", // tokenize for all suffix verbs, e.g., "ABC-mediated"
    "\\w+ation", // tokenize for all nominalization of simple events, e.g., "p53-phosphorylation"
    "(in)?dependent", "deficient", "response", "protein", "by", "specific", "like",
    "inducible", "responsive", "gene", "mRNA", "transcription", "cytoplasmic",
    "sensitive", "bound", "driven", "positive", "negative", "dominant",
    "family", "resistant", "activity", "proximal", "defective",
    "selective", "reporter", "fragment", "rich", "expression", // new suffixes from BC2
    "mechanisms?", "agonist", "heterozygous", "homozygous")

  // "non" is excluded from this set; it should be tokenized because it drives negation detection
  private val COMMON_PREFIXES = Set("anti", "auto", "bi", "co", "de", "dis", "down", "extra", "homo", "hetero", "hyper", "macro", "micro", "mono", "omni", "over", "poly", "pre", "post", "re", "semi", "sub", "super", "trans", "under", "up")

  private val dashSuffixes = mkDashSuffixes

  // A valid protein name must start with a letter, and contain at least 3 \w characters overall
  private val VALID_PROTEIN = "[a-z][\\w\\-][\\w\\-]+"
  private val VALID_PROTEIN_NO_DASH = "[a-z][\\w][\\w]+"
  private val VALID_COMPLEX_SEPARATOR_PATTERN = Pattern.compile("[/\\-]")

  private val SINGLESLASH_PATTERN = Pattern.compile(s"($VALID_PROTEIN)(/)($VALID_PROTEIN)", Pattern.CASE_INSENSITIVE)
  private val SINGLEDASH_PATTERN = Pattern.compile(s"($VALID_PROTEIN_NO_DASH)(\\-)($VALID_PROTEIN_NO_DASH)", Pattern.CASE_INSENSITIVE)

  private val SITE1_PATTERN = Pattern.compile("[ACDEFGHIKLMNQRSTVWY]\\d+", Pattern.CASE_INSENSITIVE)
  private val SITE2 = "glycine|phenylalanine|leucine|serine|tyrosine|cysteine|tryptophan|proline|histidine|arginine|soleucine|methionine|threonine|asparagine|lysine|serine|arginine|valine|alanine|aspartate|glutamate|glycine"
  private val SITE3 = "Ala|Arg|Asn|Asp|Cys|Gln|Glu|Gly|His|Ile|Leu|Lys|Met|Phe|Pro|Ser|Thr|Trp|Tyr|Val"
  private val SITE2_PATTERN = Pattern.compile(SITE2, Pattern.CASE_INSENSITIVE)
  private val SITE3_PATTERN = Pattern.compile(s"($SITE3)\\d*", Pattern.CASE_INSENSITIVE)
  private val MUTATION1_PATTERN = Pattern.compile("[ACDEFGHIKLMNQRSTVWY]\\d+[ACDEFGHIKLMNPQRSTVWY]", Pattern.CASE_INSENSITIVE)
  private val MUTATION2_PATTERN = Pattern.compile("P\\d+[ACDEFGHIKLMNPQRSTVWYacdefghiklmnpqrstvwy]")
  private val MUTATION3_PATTERN = Pattern.compile("(Ala|Arg|Asn|Asp|Cys|Gln|Glu|Gly|His|Ile|Leu|Lys|Met|Phe|Pro|Ser|Thr|Trp|Tyr|Val)\\d+(Ala|Arg|Asn|Asp|Cys|Gln|Glu|Gly|His|Ile|Leu|Lys|Met|Phe|Pro|Ser|Thr|Trp|Tyr|Val)", Pattern.CASE_INSENSITIVE)
  private val MODIFICATIONS = Set(SITE1_PATTERN, SITE2_PATTERN, SITE3_PATTERN, MUTATION1_PATTERN, MUTATION2_PATTERN, MUTATION3_PATTERN)

  private val sitePrefixes = Pattern.compile(s"($SITE2|$SITE3)(-)([\\w/]+)", Pattern.CASE_INSENSITIVE)

  private val PARENS = Set("(", ")", "[", "]")

  private val COMPLEX = Pattern.compile("complex|dimer|heterodimer")
  private val MUTANT = Pattern.compile("mutant|mutants|mutation|mutations")

  private val MEASUREMENT_UNIT_WITHSLASH = Pattern.compile("\\w+/(ml|l|cm|m)", Pattern.CASE_INSENSITIVE)

  private def isParen(s:String) = PARENS.contains(s)

  private def isMeasurementUnit(s:String):Boolean = MEASUREMENT_UNIT_WITHSLASH.matcher(s).matches()

  private def isComplex(word:String):Boolean = {
    val m = COMPLEX.matcher(word)
    m.matches()
  }
  private def isMutant(word:String):Boolean = {
    val m = MUTANT.matcher(word)
    m.matches()
  }

  private def isValidProtein(word:String):Boolean = {
    val m = Pattern.compile(VALID_PROTEIN, Pattern.CASE_INSENSITIVE).matcher(word)
    m.matches()
  }

  private def countConnectingTokens(tokens:Array[PostProcessorToken], offset:Int, howManyConnecting:MutableNumber[Int]):Boolean = {
    var i = offset
    while(i < tokens.length - 1 && isConnectingToken(tokens(i).word) && // found connecting token(s) that were tokenized too aggressively
      tokens(i - 1).endPosition == tokens(i).beginPosition && // attached to the previous token
      tokens(i).endPosition == tokens(i + 1).beginPosition) { // attached to the next token
      howManyConnecting.value += 1
      i += 1
    }

    howManyConnecting.value > 0
  }

  private def isConnectingToken(word:String):Boolean = {
    // is "/" or "-" or "-" followed by some digit(s)
    if(Pattern.compile("[\\-/]|\\-\\d+", Pattern.CASE_INSENSITIVE).matcher(word).matches())
      return true

    // starts with "-" digit+ "-" or "/"
    if(Pattern.compile("^\\-\\d+[\\-/]", Pattern.CASE_INSENSITIVE).matcher(word).find())
      return true

    false
  }

  private def mkDashSuffixes:Pattern = {
    val allSuffixes = makeRegexOr(VALID_DASH_SUFFIXES)
    val allSuffixesRegex = "([\\w/]+)(-)(" + allSuffixes + ")"
    Pattern.compile(allSuffixesRegex, Pattern.CASE_INSENSITIVE)
  }

  private def makeRegexOr(pieces: Set[String]):String = {
    val suffixBuilder = new StringBuilder()
    for (suffix <- pieces) {
      if (suffixBuilder.nonEmpty) suffixBuilder.append("|")
      suffixBuilder.append("(" + suffix + ")")
    }
    suffixBuilder.toString()
  }
}

case class PostProcessorToken(word:String, beginPosition:Int, endPosition:Int)
