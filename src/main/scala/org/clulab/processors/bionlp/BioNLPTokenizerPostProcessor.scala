package org.clulab.processors.bionlp

import java.util.regex.Pattern

import edu.stanford.nlp.ling.CoreLabel
import BioNLPTokenizerPostProcessor._
import edu.stanford.nlp.process.CoreLabelTokenFactory

import scala.StringBuilder
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Post processes CoreNLP tokenization so it suits bio analysis
 * User: mihais
 * Date: 11/16/14
 */
class BioNLPTokenizerPostProcessor {

  def process(input:Array[CoreLabel]):Array[CoreLabel] = {
    var tokens = input

    // reattach / that is attached to prev/next tokens; CoreNLP is too aggressive on slash tokenization
    tokens = reattachSlash(tokens)

    // non is a special prefix, because it drives negation detection
    tokens = breakOnPattern(tokens, Pattern.compile("(non)(-)(\\w+)", Pattern.CASE_INSENSITIVE))

    // tokenize around "-" when the suffix is a known verb, noun, or other important word
    tokens = breakOnPattern(tokens, dashSuffixes)

    // break binary complexes
    tokens = breakComplex(tokens, SINGLEDASH_PATTERN)

    // break mutations
    // TODO: this needs improvement, see Dane's comments
    tokens = breakMutant(tokens, SINGLEDASH_PATTERN)

    // break all (well, most) tokens containing a single slash; try to replace them with an enumeration
    tokens = breakOneSlash(tokens, SINGLESLASH_PATTERN)

    if(CONTEXTUAL_COMPLEX_TOKENIZATION) {
      // break complexes if the parts appear as distinct tokens somewhere else in the sequence of tokens
      // for example, this breaks "A-B-C complex" into "A, B, and C complex" if "A", "B", and "C" appear
      // as distinct tokens somewhere else in this document

      // find unique tokens
      val uniqueTokens = new mutable.HashSet[String]()
      for(t <- tokens) uniqueTokens += t.word()

      tokens = breakComplexesUsingContext(tokens, uniqueTokens.toSet)
    }

    // re-join trailing or preceding - or + to previous digit
    tokens = joinSigns(tokens)

    tokens
  }

  def breakOnPattern(tokens:Array[CoreLabel], pattern:Pattern):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    for(i <- tokens.indices) {
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

//    print("Before breakOneSlash:")
//    for(i <- tokens.indices) print(" " + tokens(i).word())
//    println

    for(i <- tokens.indices) {
      val token = tokens(i)
      val matcher = pattern.matcher(token.word())
      if (matcher.matches() && ! isMeasurementUnit(token.word())) {
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

//    print("After breakOneSlash:")
//    for(i <- output.indices) print(" " + output(i).word())
//    println

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

  def breakComplex(tokens:Array[CoreLabel], pattern:Pattern):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    for(i <- tokens.indices) {
      val token = tokens(i)
      val matcher = pattern.matcher(token.word())
      if (matcher.matches() && // contains a dash or some known separator
        ((i < tokens.length - 1 && isComplex(tokens(i + 1).word())) || // followed by "complex", or
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

  def breakComplexesUsingContext(tokens:Array[CoreLabel], uniqueTokens:Set[String]):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    for(i <- tokens.indices) {
      val token = tokens(i)
      val word = token.word()
      val sepMatcher = VALID_COMPLEX_SEPARATOR_PATTERN.matcher(token.word())
      if (sepMatcher.find() && // contains a dash or some known separator
          ((i < tokens.length - 1 && isComplex(tokens(i + 1).word())) || // followed by "complex", or
           (i > 0 && isComplex(tokens(i - 1).word())))){ // preceded by "complex"

        // start breaking down the token into sub-tokens based on separators such as "-" or "/"
        //println("Tokenizing " + word + " with other tokens: " + uniqueTokens)
        var offset = 0 // for the matcher
        var tokenStart = 0 // where the current sub-token starts
        sepMatcher.reset()
        val subTokens = new ArrayBuffer[(String, Int, Int)]() // token, start, length

        // find all valid sub-tokens inside this word, e.g., "Mek/Ras/Akt1" is broken into "Mek", "Ras", and "Akt1"
        //   *if* the corresponding sub-tokens appear standalone somewhere else in the text (i.e., in uniqueTokens)
        while(sepMatcher.find(offset)) {
          val sepPos = sepMatcher.start()
          val subToken = word.substring(tokenStart, sepPos)
          if(uniqueTokens.contains(subToken)) {
            val t = new Tuple3(subToken, token.beginPosition() + tokenStart, sepPos - tokenStart)
            subTokens += t
            //println("\tsubToken: " + t)
            tokenStart = sepMatcher.end()
          }
          offset = sepMatcher.end()
        }
        // left over at the end of the word
        if(tokenStart < word.length) {
          val subToken = word.substring(tokenStart)
          val t = new Tuple3(subToken, token.beginPosition() + tokenStart, word.length - tokenStart)
          subTokens += t
          //println("\tsubToken: " + t)
        }

        // now create actual tokens from all these sub-tokens
        for(i <- subTokens.indices) {
          // add a "," or "and" before each non-start token
          if(i > 0) {
            val prev = subTokens(i - 1)
            if(i < subTokens.length - 1) {
              output += tokenFactory.makeToken(",", prev._2 + prev._3, 1)
            } else {
              output += tokenFactory.makeToken("and", prev._2 + prev._3, 1)
            }
          }
          // add the actual token
          val crt = subTokens(i)
          output += tokenFactory.makeToken(crt._1, crt._2, crt._3)
        }

      } else {
        output += token
      }
    }
    output.toArray
  }

  def breakMutant(tokens:Array[CoreLabel], pattern:Pattern):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    for(i <- tokens.indices) {
      val token = tokens(i)
      val matcher = pattern.matcher(token.word())
      if (matcher.matches() && // contains a dash or some known separator
        ((i < tokens.length - 1 && isMutant(tokens(i + 1).word())) || // followed by "mutant", or
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

  def reattachSlash(tokens:Array[CoreLabel]):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    var crtToken = new StringBuilder
    var crtTokenBeginPosition = 0
    var i = 0
    while (i < tokens.length) {
      if(i > 0 && i < tokens.length - 1 &&
         tokens(i).word() == "/" && // found a slash
         tokens(i - 1).endPosition == tokens(i).beginPosition && // attached to the previous token
         tokens(i).endPosition == tokens(i + 1).beginPosition) { // attached to the next token
        // found an aggressive separation for this slash; revert it
        crtToken.append(tokens(i).word())
        crtToken.append(tokens(i + 1).word())
        i += 2
      } else {
        if(crtToken.size > 0) {
          val word = crtToken.toString()
          output += tokenFactory.makeToken(word, crtTokenBeginPosition, word.length)
          crtToken = new StringBuilder
        }
        crtToken.append(tokens(i).word())
        crtTokenBeginPosition = tokens(i).beginPosition
        i += 1
      }
    }
    if(crtToken.size > 0) {
      val word = crtToken.toString()
      output += tokenFactory.makeToken(word, crtTokenBeginPosition, word.length)
    }
    output.toArray
  }

  def joinSigns(tokens:Array[CoreLabel]):Array[CoreLabel] = {
    val output = new ArrayBuffer[CoreLabel]
    var i = 0
    while(i < tokens.length) {
      // -/-
      if(i < tokens.length - 3 &&
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
      } else if(i < tokens.length - 1) {
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
          (i + 2 >= tokens.length || nxt.endPosition != tokens(i + 2).beginPosition) &&
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
  val CONTEXTUAL_COMPLEX_TOKENIZATION = true

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

  val VALID_PROTEIN = "[a-z][\\w\\-_]+"
  val VALID_PROTEIN_NO_DASH = "[a-z][\\w_]+"
  val VALID_COMPLEX_SEPARATOR_PATTERN = Pattern.compile("[/\\-]")

  val SINGLESLASH_PATTERN = Pattern.compile(s"($VALID_PROTEIN)(/)($VALID_PROTEIN)", Pattern.CASE_INSENSITIVE)
  val SINGLEDASH_PATTERN = Pattern.compile(s"($VALID_PROTEIN_NO_DASH)(\\-)($VALID_PROTEIN_NO_DASH)", Pattern.CASE_INSENSITIVE)

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

  val MEASUREMENT_UNIT_WITHSLASH = Pattern.compile("\\w+/(ml|l|cm|m)", Pattern.CASE_INSENSITIVE)

  def isParen(s:String) = PARENS.contains(s)

  def isMeasurementUnit(s:String):Boolean = MEASUREMENT_UNIT_WITHSLASH.matcher(s).matches()

  def mkDashSuffixes:Pattern = {
    val allSuffixes = makeRegexOr(VALID_DASH_SUFFIXES)
    val allSuffixesRegex = "([\\w/]+)(-)(" + allSuffixes + ")"
    Pattern.compile(allSuffixesRegex, Pattern.CASE_INSENSITIVE)
  }

  def makeRegexOr(pieces: Set[String]):String = {
    val suffixBuilder = new StringBuilder()
    for (suffix <- pieces) {
      if (suffixBuilder.nonEmpty) suffixBuilder.append("|")
      suffixBuilder.append("(" + suffix + ")")
    }
    suffixBuilder.toString()
  }
}
