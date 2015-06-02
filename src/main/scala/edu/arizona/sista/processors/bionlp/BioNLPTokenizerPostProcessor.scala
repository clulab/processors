package edu.arizona.sista.processors.bionlp

import java.util.regex.Pattern

import edu.stanford.nlp.ling.CoreLabel
import BioNLPTokenizerPostProcessor._
import edu.stanford.nlp.process.CoreLabelTokenFactory

import scala.collection.mutable.ArrayBuffer

/**
 * Post processes CoreNLP tokenization so it suits bio analysis
 * User: mihais
 * Date: 11/16/14
 */
class BioNLPTokenizerPostProcessor {

  def process(input:Array[CoreLabel]):Array[CoreLabel] = {
    var tokens = input

    // we can't use the regex "(anti)|(non)" since that will create an extra
    // group and confuse breakOnPattern, thus we do an extra pass
    tokens = breakOnPattern(tokens, Pattern.compile("(anti)(-)(\\w+)", Pattern.CASE_INSENSITIVE))
    tokens = breakOnPattern(tokens, Pattern.compile("(non)(-)(\\w+)", Pattern.CASE_INSENSITIVE))
    tokens = breakOnPattern(tokens, dashSuffixes)
    tokens = breakOneSlash(tokens, SINGLESLASH_PATTERN)
    tokens = breakComplex(tokens, SINGLEDASH_PATTERN)

    // re-join trailing or preceding - or + to previous digit
    tokens = joinSigns(tokens)

    tokens
  }

  def breakOnPattern(tokens:Array[CoreLabel], pattern:Pattern):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    for(i <- 0 until tokens.size) {
      val token = tokens(i)
      val matcher = pattern.matcher(token.word())
      if (matcher.matches()) {
        val sepPos = matcher.start(2)
        val s1 = token.word().substring(0, sepPos)
        if(ACTUAL_PREFIXES.contains(s1.toLowerCase)) {
          // do not separate here; these prefixes cannot live on their own
          output += token
        } else {
          if (!DISCARD_STANDALONE_DASHES || !s1.equals("-")) {
            output += tokenFactory.makeToken(s1, token.beginPosition(), sepPos)
          }
          val sep = matcher.group(2)
          if (!DISCARD_STANDALONE_DASHES || !sep.equals("-")) {
            output += tokenFactory.makeToken(sep, token.beginPosition() + sepPos, 1)
          }
          val s3 = token.word().substring(sepPos + 1)
          if (!DISCARD_STANDALONE_DASHES || !s3.equals("-")) {
            output += tokenFactory.makeToken(s3, token.beginPosition() + sepPos + 1,
              token.endPosition() - token.beginPosition() - sepPos - 1)
          }
        }
      } else {
        output += token
      }
    }
    output.toArray
  }

  def breakOneSlash(tokens:Array[CoreLabel], pattern:Pattern):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    for(i <- 0 until tokens.size) {
      val token = tokens(i)
      val matcher = pattern.matcher(token.word())
      if (matcher.matches()) {
        val sepPos = matcher.start(2)
        val s1 = token.word().substring(0, sepPos)
        output += tokenFactory.makeToken(s1, token.beginPosition(), sepPos)
        output += tokenFactory.makeToken("and", token.beginPosition() + sepPos, 1) // replace "/" with "and"; it parses better
        val s3 = token.word().substring(sepPos + 1)
        output += tokenFactory.makeToken(s3, token.beginPosition() + sepPos + 1,
          token.endPosition() - token.beginPosition() - sepPos - 1)
      } else {
        output += token
      }
    }
    output.toArray
  }

  def breakComplex(tokens:Array[CoreLabel], pattern:Pattern):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    for(i <- 0 until tokens.size) {
      val token = tokens(i)
      val matcher = pattern.matcher(token.word())
      if (matcher.matches() && i < tokens.size - 1 && isComplex(tokens(i + 1).word().toLowerCase())) {
        val sepPos = matcher.start(2)
        val s1 = token.word().substring(0, sepPos)
        output += tokenFactory.makeToken(s1, token.beginPosition(), sepPos)
        output += tokenFactory.makeToken("and", token.beginPosition() + sepPos, 1) // replace "-" with "and"; it parses better
        val s3 = token.word().substring(sepPos + 1)
        output += tokenFactory.makeToken(s3, token.beginPosition() + sepPos + 1,
          token.endPosition() - token.beginPosition() - sepPos - 1)
      } else {
        output += token
      }
    }
    output.toArray
  }

  def joinSigns(tokens:Array[CoreLabel]):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    var i = 0
    while(i < tokens.size) {
      // -/-
      if(i < tokens.size - 3 &&
        tokens(i).endPosition == tokens(i + 1).beginPosition &&
        tokens(i + 1).word == "-" &&
        tokens(i + 2).word == "/" &&
        tokens(i + 3).word == "-"){
        val word = tokens(i).word +
          tokens(i + 1).word +
          tokens(i + 2).word +
          tokens(i + 3).word
        output += tokenFactory.makeToken(word, tokens(i).beginPosition, word.length)
        i += 4

      // - or +
      } else if(i < tokens.size - 1) {
        val crt = tokens(i)
        val nxt = tokens(i + 1)

        // trailing +
        if(crt.endPosition == nxt.beginPosition &&
          ! isParen(crt.word) && nxt.word == "+"){
          val word = crt.word + nxt.word
          output += tokenFactory.makeToken(word, crt.beginPosition, word.length)
          i += 2
        }

        // trailing -
        else if(crt.endPosition == nxt.beginPosition &&
          (i + 2 >= tokens.size || nxt.endPosition != tokens(i + 2).beginPosition) &&
          ! isParen(crt.word) && nxt.word == "-"){
          val word = crt.word + nxt.word
          output += tokenFactory.makeToken(word, crt.beginPosition, word.length)
          i += 2
        }

        // preceding -
        else if(crt.endPosition == nxt.beginPosition &&
          (i == 0 || crt.beginPosition != tokens(i - 1).endPosition) &&
          ! isParen(nxt.word) && crt.word() == "-"){
          val word = crt.word + nxt.word
          output += tokenFactory.makeToken(word, crt.beginPosition, word.length)
          i += 2

        // nothing interesting
        } else {
          output += tokens(i)
          i += 1
        }

      // nothing interesting
      } else {
        output += tokens(i)
        i += 1
      }
    }
    output.toArray
  }

  def isComplex(word:String):Boolean = {
    val m = COMPLEX.matcher(word)
    m.matches()
  }
}

object BioNLPTokenizerPostProcessor {
  val tokenFactory = new CoreLabelTokenFactory()

  val DISCARD_STANDALONE_DASHES = true

  val VALID_DASH_SUFFIXES = Set(
    "\\w+ed", "\\w+ing", // tokenize for all suffix verbs, e.g., "ABC-mediated"
    "(in)?dependent", "deficient", "response", "protein", "by", "specific", "like",
    "inducible", "responsive", "gene", "mRNA", "transcription", "cytoplasmic",
    "sensitive", "bound", "driven", "positive", "negative", "dominant",
    "family", "resistant", "activity", "proximal", "defective",
    "selective", "reporter", "fragment", "rich", "expression", // new suffixes from BC2
    "mechanisms?", "agonist", "heterozygous", "homozygous")

  val ACTUAL_PREFIXES = Set("co", "semi")

  val dashSuffixes = mkDashSuffixes

  val SINGLESLASH_PATTERN = Pattern.compile("([\\w\\-_]+)(/)([\\w\\-_]+)", Pattern.CASE_INSENSITIVE)
  val SINGLEDASH_PATTERN = Pattern.compile("([\\w\\-_]+)(\\-)([\\w\\-_]+)", Pattern.CASE_INSENSITIVE)

  val PARENS = Set("(", ")", "[", "]")

  val COMPLEX = Pattern.compile("complex|dimer|heterodimer")

  def isParen(s:String) = PARENS.contains(s)

  def mkDashSuffixes:Pattern = {
    val allSuffixes = makeRegexOr(VALID_DASH_SUFFIXES)
    val allSuffixesRegex = "(\\w+)(-)(" + allSuffixes + ")"
    Pattern.compile(allSuffixesRegex, Pattern.CASE_INSENSITIVE)
  }

  def makeRegexOr(pieces: Set[String]):String = {
    val suffixBuilder = new StringBuilder()
    for (suffix <- pieces) {
      if (suffixBuilder.length > 0) suffixBuilder.append("|")
      suffixBuilder.append("(" + suffix + ")")
    }
    suffixBuilder.toString()
  }
}
