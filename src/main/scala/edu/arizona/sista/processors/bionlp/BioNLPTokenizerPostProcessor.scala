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

    // non is a special prefix, because it drives negation detection
    tokens = breakOnPattern(tokens, Pattern.compile("(non)(-)(\\w+)", Pattern.CASE_INSENSITIVE))

    // we now handle common English prefixes much better; see COMMON_PREFIXES
    // tokens = breakOnPattern(tokens, Pattern.compile("(anti)(-)(\\w+)", Pattern.CASE_INSENSITIVE))

    // tokenize around "-" shen the suffix is a known verb, noun, or other important word
    tokens = breakOnPattern(tokens, dashSuffixes)

    // break binary complexes
    tokens = breakComplex(tokens, SINGLEDASH_PATTERN)
    // break mutations
    tokens = breakMutant(tokens, SINGLEDASH_PATTERN)

    if(AGGRESSIVE_SLASH_TOKENIZATION) {
      // break all tokens containing slashes; try to replace them with an enumeration
      tokens = breakOneSlash(tokens, SINGLESLASH_PATTERN)
    } else {
      // break / separated complexes
      tokens = breakComplex(tokens, SINGLESLASH_PATTERN)
      // tokenize around slashes for modifications
      tokens = breakTwoModifications(tokens, SINGLESLASH_PATTERN)
      tokens = breakThreeModifications(tokens, DOUBLESLASH_PATTERN)
    }

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
        if(COMMON_PREFIXES.contains(s1.toLowerCase)) {
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
        val s3 = token.word().substring(sepPos + 1)

        output += tokenFactory.makeToken(s1, token.beginPosition(), sepPos)
        output += tokenFactory.makeToken("and", token.beginPosition() + sepPos, 1) // replace "/" with "and"; it parses better
        output += tokenFactory.makeToken(s3, token.beginPosition() + sepPos + 1,
          token.endPosition() - token.beginPosition() - sepPos - 1)
      } else {
        output += token
      }
    }
    output.toArray
  }

  /** True if this a Site or some Mutation */
  def isModification(s:String):Boolean = {
    for(p <- MODIFICATIONS) {
      val m = p.matcher(s)
      if(m.matches()) return true
    }
    false
  }

  def breakTwoModifications(tokens:Array[CoreLabel], pattern:Pattern):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    for(i <- 0 until tokens.size) {
      val token = tokens(i)
      val matcher = pattern.matcher(token.word())
      if (matcher.matches()) {
        val sepPos = matcher.start(2)
        val s1 = token.word().substring(0, sepPos)
        val s3 = token.word().substring(sepPos + 1)

        if(isModification(s1) && isModification(s3)) {
          output += tokenFactory.makeToken(s1, token.beginPosition(), sepPos)
          output += tokenFactory.makeToken("and", token.beginPosition() + sepPos, 1) // replace "/" with "and"; it parses better
          output += tokenFactory.makeToken(s3, token.beginPosition() + sepPos + 1, token.endPosition() - token.beginPosition() - sepPos - 1)
        }
      } else {
        output += token
      }
    }
    output.toArray
  }
  def breakThreeModifications(tokens:Array[CoreLabel], pattern:Pattern):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    for(i <- 0 until tokens.size) {
      val token = tokens(i)
      val matcher = pattern.matcher(token.word())
      if (matcher.matches()) {
        val sepPos1 = matcher.start(2)
        val sepPos2 = matcher.start(4)
        val s1 = token.word().substring(0, sepPos1)
        val s3 = token.word().substring(sepPos1 + 1, sepPos2)
        val s5 = token.word().substring(sepPos2 + 1)

        if(isModification(s1) && isModification(s3) && isModification(s5)) {
          output += tokenFactory.makeToken(s1, token.beginPosition(), sepPos1)
          output += tokenFactory.makeToken(",", token.beginPosition() + sepPos1, 1) // replace first "/" with ","; it parses better
          output += tokenFactory.makeToken(s3, token.beginPosition() + sepPos1 + 1, sepPos2 - sepPos1 - 1)
          output += tokenFactory.makeToken(",", token.beginPosition() + sepPos2, 1) // replace second "/" with "," "and"; it parses better
          output += tokenFactory.makeToken("and", token.beginPosition() + sepPos2, 1)
          output += tokenFactory.makeToken(s5, token.beginPosition() + sepPos2 + 1, token.endPosition() - token.beginPosition() - sepPos2 - 1)
        }
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
      if (matcher.matches() && // contains a dash or some known separator
        ((i < tokens.size - 1 && isComplex(tokens(i + 1).word())) || // followed by "complex", or
          (i > 0 && isComplex(tokens(i - 1).word())))){ // preceded by "complex"
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

  def breakMutant(tokens:Array[CoreLabel], pattern:Pattern):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    for(i <- 0 until tokens.size) {
      val token = tokens(i)
      val matcher = pattern.matcher(token.word())
      if (matcher.matches() && // contains a dash or some known separator
        ((i < tokens.size - 1 && isMutant(tokens(i + 1).word())) || // followed by "mutant", or
          (i > 0 && isMutant(tokens(i - 1).word())))){ // preceded by mutant
        val sepPos = matcher.start(2)
        val s1 = token.word().substring(0, sepPos)
        output += tokenFactory.makeToken(s1, token.beginPosition(), sepPos)
        // "-" is simply removed for mutation modifications
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
  def isMutant(word:String):Boolean = {
    val m = MUTANT.matcher(word)
    m.matches()
  }
}

object BioNLPTokenizerPostProcessor {
  val tokenFactory = new CoreLabelTokenFactory()

  val DISCARD_STANDALONE_DASHES = true
  val AGGRESSIVE_SLASH_TOKENIZATION = true

  val VALID_DASH_SUFFIXES = Set(
    "\\w+ed", "\\w+ing", // tokenize for all suffix verbs, e.g., "ABC-mediated"
    "\\w+ation", // tokenize for all nominalization of simple events, e.g., "p53-phosphorylation"
    "(in)?dependent", "deficient", "response", "protein", "by", "specific", "like",
    "inducible", "responsive", "gene", "mRNA", "transcription", "cytoplasmic",
    "sensitive", "bound", "driven", "positive", "negative", "dominant",
    "family", "resistant", "activity", "proximal", "defective",
    "selective", "reporter", "fragment", "rich", "expression", // new suffixes from BC2
    "mechanisms?", "agonist", "heterozygous", "homozygous")

  // "non" is excluded from this set; it should be tokenized because it drives negation detection
  val COMMON_PREFIXES = Set("anti", "auto", "bi", "co", "de", "dis", "extra", "homo", "hetero", "hyper", "macro", "micro", "mono", "omni", "over", "poly", "pre", "post", "re", "semi", "sub", "super", "trans", "under")

  val dashSuffixes = mkDashSuffixes

  val SINGLESLASH_PATTERN = Pattern.compile("([\\w\\-_]+)(/)([\\w\\-_]+)", Pattern.CASE_INSENSITIVE)
  val DOUBLESLASH_PATTERN = Pattern.compile("([\\w\\-_]+)(/)([\\w\\-_]+)(/)([\\w\\-_]+)", Pattern.CASE_INSENSITIVE)

  val SINGLEDASH_PATTERN = Pattern.compile("([\\w_]+)(\\-)([\\w_]+)", Pattern.CASE_INSENSITIVE)

  val SITE1 = Pattern.compile("[ACDEFGHIKLMNQRSTVWY]\\d+", Pattern.CASE_INSENSITIVE)
  val SITE2 = Pattern.compile("glycine|phenylalanine|leucine|serine|tyrosine|cysteine|tryptophan|proline|histidine|arginine|soleucine|methionine|threonine|asparagine|lysine|serine|arginine|valine|alanine|aspartate|glutamate|glycine", Pattern.CASE_INSENSITIVE)
  val SITE3 = Pattern.compile("(Ala|Arg|Asn|Asp|Cys|Gln|Glu|Gly|His|Ile|Leu|Lys|Met|Phe|Pro|Ser|Thr|Trp|Tyr|Val)\\d*", Pattern.CASE_INSENSITIVE)
  val MUTATION1 = Pattern.compile("[ACDEFGHIKLMNQRSTVWY]\\d+[ACDEFGHIKLMNPQRSTVWY]", Pattern.CASE_INSENSITIVE)
  val MUTATION2 = Pattern.compile("P\\d+[ACDEFGHIKLMNPQRSTVWYacdefghiklmnpqrstvwy]")
  val MUTATION3 = Pattern.compile("(Ala|Arg|Asn|Asp|Cys|Gln|Glu|Gly|His|Ile|Leu|Lys|Met|Phe|Pro|Ser|Thr|Trp|Tyr|Val)\\d+(Ala|Arg|Asn|Asp|Cys|Gln|Glu|Gly|His|Ile|Leu|Lys|Met|Phe|Pro|Ser|Thr|Trp|Tyr|Val)", Pattern.CASE_INSENSITIVE)
  val MODIFICATIONS = Set(SITE1, SITE2, SITE3, MUTATION1, MUTATION2, MUTATION3)

  val PARENS = Set("(", ")", "[", "]")

  val COMPLEX = Pattern.compile("complex|dimer|heterodimer")
  val MUTANT = Pattern.compile("mutant|mutants|mutation|mutations")

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
