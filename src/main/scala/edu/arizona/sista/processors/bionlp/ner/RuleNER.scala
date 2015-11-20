package edu.arizona.sista.processors.bionlp.ner

import java.io.{InputStreamReader, BufferedReader}

import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.struct.HashTrie
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import RuleNER._

/**
 * Rule-based NER for the Bio domain
 * If useLemmas is true, tokens are matched using lemmas, otherwise using words
 * knownCaseInsensitives contains single-token entities that can be spelled using lower case, according to the KB(s)
 * User: mihais
 * Date: 5/11/15
 */
class RuleNER(val matchers:Array[(String, HashTrie)], val knownCaseInsensitives:Set[String], val useLemmas:Boolean = false) {

  def this(matchers:Array[(String, HashTrie)], useLemmas:Boolean) { this(matchers, Set[String](), useLemmas) }

  def find(sentence:Sentence):Array[String] = {
    // findByPriority(sentence)
    val seq = findLongestMatch(sentence)
    seq
  }

  def getTokens(sentence:Sentence):Array[String] = {
    useLemmas match {
      case true => sentence.lemmas.get
      case _ => sentence.words
    }
  }

  /**
   * Finds the longest match across all matchers.
   * This means that the longest match is always chosen, even if coming from a matcher with lower priority
   * Only ties are disambiguated according to the order provided in the constructor
   */
  def findLongestMatch(sentence:Sentence):Array[String] = {
    val tokens = getTokens(sentence)
    val caseInsensitiveWords = tokens.map(_.toLowerCase)

    var offset = 0
    val labels = new ArrayBuffer[String]()
    while(offset < tokens.length) {
      // stores the spans found by all matchers
      val spans = new Array[Int](matchers.length)

      // attempt to match each category at this offset
      for (i <- matchers.indices) {
        spans(i) = findAt(tokens, caseInsensitiveWords, matchers(i)._2, offset, sentence)
      }

      // pick the longest match
      // solve ties by preferring earlier (higher priority) matchers
      var bestSpanOffset = -1
      var bestSpan = -1
      for(i <- matchers.indices) {
        if(spans(i) > bestSpan) {
          bestSpanOffset = i
          bestSpan = spans(i)
        }
      }

      // found something!
      if(bestSpanOffset != -1) {
        assert(bestSpan > 0)
        val label = matchers(bestSpanOffset)._1
        labels += "B-" + label
        for(i <- 1 until bestSpan) {
          labels += "I-" + label
        }
        offset += bestSpan
      } else {
        labels += OUTSIDE_LABEL
        offset += 1
      }
    }

    labels.toArray
  }

  private def findAt(seq:Array[String],
                     caseInsensitiveSeq:Array[String],
                     matcher:HashTrie,
                     offset:Int,
                     sentence:Sentence):Int = {
    val span = matcher.caseInsensitive match {
      case true => matcher.findAt(caseInsensitiveSeq, offset)
      case _ => matcher.findAt(seq, offset)
    }

    if(span > 0 && validMatch(offset, offset + span, sentence)) span
    else -1
  }

  private def validMatch(start:Int, end:Int, sentence:Sentence):Boolean = {
    // must contain at least one NN*
    // see also removeSinglePrepositions, for deprecated code
    var nouns = 0
    for(i <- start until end)
      if(sentence.tags.get(i).startsWith("NN"))
        nouns += 1
    if(nouns == 0)
      return false

    // the text must contain at least one letter AND (the letter must be upper case OR the text contains at least 1 digit)
    val text = sentence.getSentenceFragmentText(start, end)
    val (letters, digits, upperCaseLetters, spaces) = scanText(text)
    if(letters > 0 && (digits > 0 || upperCaseLetters > 0 || spaces > 0)) {
      //println("Found valid match: " + text)
      return true
    }

    // have we seen this single token as lower case in the KB; if so, accept it in the text
    if(letters > 0 && knownCaseInsensitives.contains(text)) {
      return true
    }

    // if at least 1 letter and length > 3 accept (e.g., "rapamycin")
    if(letters > 0 && text.length > 3)
      return true

    false
  }

  private def scanText(text:String):(Int, Int, Int, Int) = {
    var letters = 0
    var digits = 0
    var upperCaseLetters = 0
    var spaces = 0
    for(i <- text.indices) {
      val c = text.charAt(i)
      if(Character.isLetter(c)) letters += 1
      if(Character.isUpperCase(c)) upperCaseLetters += 1
      if(Character.isDigit(c)) digits += 1
      if(Character.isWhitespace(c)) spaces += 1
    }
    (letters, digits, upperCaseLetters, spaces)
  }

  /**
   * Inspects matchers in the order provided in the constructor
   * This means that a matcher with higher priority is preferred even if a longer one (with lower priority) exists!
   */
  def findByPriority(sentence:Sentence):Array[String] = {
    val overallLabels = new Array[String](sentence.size)
    for(i <- overallLabels.indices) overallLabels(i) = OUTSIDE_LABEL
    val tokens = getTokens(sentence)
    for(matcher <- matchers) {
      // the actual match
      var labels = matcher._2.find(tokens, matcher._1, OUTSIDE_LABEL)

      // some matchers are overmatching due to the case-insensitive setting,
      // e.g., Gene_or_gene_product contains protein names that are identical to prepositions, such as "IN" and "TO"
      labels = filterMatches(labels, sentence)
      //println("LABELS: " + labels.mkString(" "))

      // matchers must be stored in descending order of their priorities
      // so we do not allow new labels to overwrite labels already generated
      RuleNER.mergeLabels(overallLabels, labels)
    }
    overallLabels
  }

  def filterMatches(labels:Array[String], sentence:Sentence):Array[String] = {
    val filtered = removeSinglePrepositions(labels, sentence)

    filtered
  }

  /** Remove single tokens that are not tagged as nouns */
  def removeSinglePrepositions(labels:Array[String], sentence:Sentence):Array[String] = {
    val filtered = new Array[String](labels.length)
    for(i <- labels.indices) {
      if(labels(i).startsWith("B-") &&
         (i == labels.length - 1 || ! labels(i + 1).startsWith("I-")) && // single token entity
         ! sentence.tags.get(i).startsWith("NN")) { // not a noun
        filtered(i) = RuleNER.OUTSIDE_LABEL
      } else {
        filtered(i) = labels(i)
      }
    }
    filtered
  }

}

object RuleNER {
  val logger = LoggerFactory.getLogger(classOf[RuleNER])
  val OUTSIDE_LABEL = "O"

  /** Loads all KBs; KBs must be listed in descending order of their priorities */
  def load(kbs:List[String], useLemmas:Boolean = false, caseInsensitive:Boolean = true):RuleNER = {
    logger.info("Beginning to load the KBs for the rule-based bio NER...")
    val matchers = new ArrayBuffer[(String, HashTrie)]
    val knownCaseInsensitives = new mutable.HashSet[String]()
    for(kb <- kbs) {
      val name = extractKBName(kb)
      val is = RuleNER.getClass.getClassLoader.getResourceAsStream(kb)
      assert(is != null, s"Failed to find KB file $kb in the classpath!")
      val reader = new BufferedReader(new InputStreamReader(is))
      val matcher = loadKB(reader, caseInsensitive, knownCaseInsensitives)
      logger.info(s"Loaded matcher for label $name. This matchers contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
      matchers += new Tuple2(name, matcher)
      reader.close()
    }
    logger.info("KB loading completed.")
    new RuleNER(matchers.toArray, knownCaseInsensitives.toSet, useLemmas)
  }

  def loadKB(reader:BufferedReader, caseInsensitive:Boolean, knownCaseInsensitives:mutable.HashSet[String]): HashTrie = {
    val matcher = new HashTrie(caseInsensitive = caseInsensitive, internStrings = true)
    var done = false
    while(! done) {
      var line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        line = line.trim
        if(! line.startsWith("#")) {
          val tokens = line.split("\\s+")
          matcher.add(tokens)

          if(tokens.length == 1 && line.toLowerCase == line) {
            knownCaseInsensitives.add(line)
          }
        }
      }
    }
    matcher
  }

  def extractKBName(kb:String):String = {
    val slash = kb.lastIndexOf("/")
    val dot = kb.lastIndexOf('.')
    val name = kb.substring(slash + 1, dot)
    name
  }

  /** Merges labels in src into dst, without overlapping any existing labels in dst */
  def mergeLabels(dst:Array[String], src:Array[String]) {
    assert(dst.length == src.length)

    var offset = 0
    while(offset < dst.length) {
      if(src(offset) != OUTSIDE_LABEL) {
        // no overlap allowed
        if(! overlap(dst, src, offset)) {
          dst(offset) = src(offset)
          offset += 1
          while(offset < src.length && src(offset).startsWith("I-")) {
            dst(offset) = src(offset)
            offset += 1
          }
        } else {
          // overlap found; just ignore the labels in src
          offset += 1
          while(offset < src.length && src(offset).startsWith("I-")) {
            offset += 1
          }
        }
      } else {
        // nothing to merge
        offset += 1
      }
    }
  }

  def overlap(dst:Array[String], src:Array[String], offset:Int):Boolean = {
    var position = offset
    if(dst(position) != OUTSIDE_LABEL) return true
    position += 1
    while(position < src.length && src(position).startsWith("I-")) {
      if(dst(position) != OUTSIDE_LABEL) return true
      position += 1
    }
    false
  }

  /**
   * A horrible hack to keep track of entities that should not be labeled when in lower case
   * TODO: This is more a placeholder, so we remember we need a more general solution here
   */
  val NOT_ENTITY_IN_LOWER_CASE = Set("blot", "prey")
}
