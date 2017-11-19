package org.clulab.sequences

import java.io.BufferedReader

import org.clulab.processors.Sentence
import org.clulab.struct.{EntityValidator, HashTrie, TrueEntityValidator}

import scala.collection.mutable.ArrayBuffer
import LexiconNER._
import org.clulab.utils.Files.loadStreamFromClasspath
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

/**
  * Lexicon-based NER, which efficiently recognizes entities from large dictionaries
  * Note: This is a cleaned-up version of the old RuleNER.
  * Create a LexiconNER object using LexiconNER.apply() (not the c'tor, which is private).
  * Use it by calling the find() method on a single sentence.
  * See org.clulab.processors.TextLexiconNER for usage examples.
  *
  * @param matchers A map of tries to be matched for each given category label
  *                 The order of the matchers is important: it indicates priority during ties (first has higher priority)
  * @param knownCaseInsensitives Set of single-token entity names that can be spelled using lower case, according to the KB(s)
  * @param useLemmas If true, tokens are matched using lemmas, otherwise using words
  *
  * Author: mihais
  * Created: 5/11/15
  * Modified: 9/27/17 - Clean up from RuleNER into LexiconNER
  */
@SerialVersionUID(1000L)  
class LexiconNER private (
  val matchers:Array[(String, HashTrie)],
  val knownCaseInsensitives:Set[String],
  val useLemmas:Boolean,
  val entityValidator: EntityValidator) extends Tagger[String] with Serializable {

  /**
    * Matches the lexicons against this sentence
    * @param sentence The input sentence
    * @return An array of BIO notations the store the outcome of the matches
    */
  def find(sentence:Sentence):Array[String] = {
    val seq = findLongestMatch(sentence)
    seq
  }

  protected def getTokens(sentence:Sentence):Array[String] = {
    if (useLemmas) {
      sentence.lemmas.get
    } else {
      sentence.words
    }
  }

  /**
    * Finds the longest match across all matchers.
    * This means that the longest match is always chosen, even if coming from a matcher with lower priority
    * Only ties are disambiguated according to the order provided in the constructor
    */
  protected def findLongestMatch(sentence:Sentence):Array[String] = {
    val tokens = getTokens(sentence)
    val caseInsensitiveWords = tokens.map(_.toLowerCase)

    var offset = 0
    val labels = new ArrayBuffer[String]()
    while(offset < tokens.length) {
      // stores the spans found by all matchers
      val spans = new Array[Int](matchers.length)

      // attempt to match each category at this offset
      for (i <- matchers.indices) {
        spans(i) = findAt(tokens, caseInsensitiveWords, matchers(i)._2, offset, entityValidator)
        // if(spans(i) > 0) println(s"Offset $offset: Matched span ${spans(i)} for matcher ${matchers(i)._1}")
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

        if(contentfulSpan(sentence, offset, bestSpan) && // does this look like a valid entity span?
           entityValidator.validMatch(sentence, offset, offset + bestSpan)) { // domain-specific constraints on entities

          val label = matchers(bestSpanOffset)._1
          //println(s"MATCHED LABEL $label from $offset to ${offset + bestSpan} (exclusive)!")
          labels += "B-" + label
          for (_ <- 1 until bestSpan) {
            labels += "I-" + label
          }
        } else {
          for(_ <- 0 until bestSpan) {
            labels += OUTSIDE_LABEL
          }
        }
        offset += bestSpan
        //println(s"Will continue matching starting at $offset")
      } else {
        labels += OUTSIDE_LABEL
        offset += 1
      }
    }

    assert(labels.length == sentence.size)
    labels.toArray
  }

  protected def contentfulSpan(sentence: Sentence, start: Int, length: Int):Boolean = {
    val (characters, letters, digits, upperCaseLetters, spaces) =
      LexiconNER.scanText(sentence.words, start, start + length)

    // a valid span must have letters > 0 and at least one of the other properties
    if(letters > 0 &&
       ( digits > 0 ||
         upperCaseLetters > 0 ||
         spaces > 0 ||
         characters > LexiconNER.KNOWN_CASE_INSENSITIVE_LENGTH ||
         knownCaseInsensitives.contains(sentence.words(start)))) {
      return true
    }

    false
  }
  
  protected def findAt(seq:Array[String],
                       caseInsensitiveSeq:Array[String],
                       matcher:HashTrie,
                       offset:Int,
                       validator:EntityValidator):Int = {
    val span = if (matcher.caseInsensitive) {
      matcher.findAt(caseInsensitiveSeq, offset)
    } else {
      matcher.findAt(seq, offset)
    }
    span
  }

}

object LexiconNER {
  val logger: Logger = LoggerFactory.getLogger(classOf[LexiconNER])
  val OUTSIDE_LABEL: String = "O"
  val INTERN_STRINGS:Boolean = false
  val KNOWN_CASE_INSENSITIVE_LENGTH:Int = 3 // this was tuned for Reach; if changed please rerun Reach unit tests

  /**
    * Creates a LexiconNER from a list of KBs
    * Note that file name (minus the extension) for each KB becomes the name of the corresponding category.
    *   For example, /Some/Path/SomeCategory.tsv.gz yields the category name SomeCategory.
    * Each of the KBs must contain one entity name per line
    *
    * @param kbs KBs containing known entity names
    * @param overrideKBs KBs containing override labels for entity names from kbs (necessary for the bio domain)
    * @param entityValidator Filter which decides if a matched entity is valid
    * @param lexicalVariationEngine Generates alternative spellings of an entity name (necessary for the bio domain)
    * @param useLemmasForMatching If true, we use Sentence.lemmas instead of Sentence.words during matching
    * @param caseInsensitiveMatching If true, tokens are matched case insensitively
    * @return The new LexiconNER
    */
  def apply(kbs:Seq[String],
            overrideKBs:Option[Seq[String]],
            entityValidator: EntityValidator,
            lexicalVariationEngine:LexicalVariations,
            useLemmasForMatching:Boolean,
            caseInsensitiveMatching:Boolean): LexiconNER = {
    logger.info("Beginning to load the KBs for the rule-based bio NER...")
    val matchers = new ArrayBuffer[(String, HashTrie)]
    val knownCaseInsensitives = new mutable.HashSet[String]()

    // load the override KBs first, so they take priority during matching
    // in these KBs, the name of the entity must be the first token, and the label must be the last
    //   (tokenization is performed around TABs here)
    if (overrideKBs.isDefined) {
      overrideKBs.get.foreach(okb => {
        val reader = loadStreamFromClasspath(okb)
        val overrideMatchers = loadOverrideKB(reader, lexicalVariationEngine, caseInsensitiveMatching, knownCaseInsensitives)
        for(name <- overrideMatchers.keySet.toList.sorted) {
          val matcher = overrideMatchers(name)
          logger.info(s"Loaded OVERRIDE matcher for label $name. This matcher contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
          matchers += Tuple2(name, matcher)
        }
        reader.close()
      })
    }

    // load the standard KBs
    for(kb <- kbs) {
      val name = extractKBName(kb)
      val reader = loadStreamFromClasspath(kb)
      val matcher = loadKB(reader, lexicalVariationEngine, caseInsensitiveMatching, knownCaseInsensitives)
      logger.info(s"Loaded matcher for label $name. This matcher contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
      matchers += Tuple2(name, matcher)
      reader.close()
    }

    logger.info("KB loading completed.")
    new LexiconNER(matchers.toArray, knownCaseInsensitives.toSet, useLemmasForMatching, entityValidator)
  }

  /**
    * Creates a LexiconNER from a list of KBs
    * Note that file name (minus the extension) for each KB becomes the name of the corresponding category.
    *   For example, /Some/Path/SomeCategory.tsv.gz yields the category name SomeCategory.
    * Each of the KBs must contain one entity name per line
    *   
    * @param kbs KBs containing known entity names
    * @param entityValidator Filter which decides if a matched entity is valid
    * @param useLemmasForMatching If true, we use Sentence.lemmas instead of Sentence.words during matching
    * @param caseInsensitiveMatching If true, tokens are matched case insensitively 
    * @return The new LexiconNER
    */
  def apply(kbs:Seq[String],
            entityValidator: EntityValidator = new TrueEntityValidator,
            useLemmasForMatching:Boolean = false,
            caseInsensitiveMatching:Boolean = false): LexiconNER = {
    apply(kbs, None,
      entityValidator, new NoLexicalVariations,
      useLemmasForMatching, caseInsensitiveMatching)
  }

  private def loadKB(
    reader:BufferedReader,
    lexicalVariationEngine:LexicalVariations,
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): HashTrie = {
    val matcher = new HashTrie(caseInsensitive = caseInsensitive, internStrings = INTERN_STRINGS)
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        addLine(line, matcher, lexicalVariationEngine, caseInsensitive, knownCaseInsensitives)
      }
    }
    matcher
  }

  private def addLine(
    inputLine:String,
    matcher:HashTrie,
    lexicalVariationEngine:LexicalVariations,
    caseInsensitive:Boolean, 
    knownCaseInsensitives:mutable.HashSet[String]): Unit = {
    
    val line = inputLine.trim
    if(! line.startsWith("#")) {
      val tokens = line.split("\\s+")
      addWithLexicalVariations(tokens, lexicalVariationEngine, matcher)

      // keep track of all lower case ents that are single token (necessary for case-insensitive matching)
      if(tokens.length == 1 && line.toLowerCase == line && caseInsensitive) { 
        knownCaseInsensitives.add(line)
      }
    }
  }

  private def loadOverrideKB(
    reader:BufferedReader,
    lexicalVariationEngine:LexicalVariations,
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): Map[String, HashTrie] = {
    val matchers = new mutable.HashMap[String, HashTrie]()
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        addOverrideLine(line, matchers, lexicalVariationEngine, caseInsensitive, knownCaseInsensitives)
      }
    }
    matchers.toMap
  }

  private def addOverrideLine(
    inputLine:String,
    matchers:mutable.HashMap[String, HashTrie],
    lexicalVariationEngine:LexicalVariations,
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): Unit = {
    val line = inputLine.trim
    if(! line.startsWith("#")) { // skip comments starting with #
      val blocks = line.split("\t")
      if (blocks.size >= 2) {
        val entity = blocks.head   // grab the text of the named entity
        val label = blocks.last    // grab the label of the named entity

        val tokens = entity.split("\\s+")
        // keep track of all lower case ents that are single token (necessary for case-insensitive matching)
        if (tokens.length == 1 && line.toLowerCase == line && caseInsensitive) {
          knownCaseInsensitives.add(line)
        }
        val matcher = matchers.getOrElseUpdate(label,
          new HashTrie(caseInsensitive = caseInsensitive, internStrings = INTERN_STRINGS))

        addWithLexicalVariations(tokens, lexicalVariationEngine, matcher)
      }
    }
  }

  private def addWithLexicalVariations(
    tokens:Array[String],
    lexicalVariationEngine:LexicalVariations,
    matcher:HashTrie): Unit = {
    // add the original form
    matcher.add(tokens)

    // add the lexical variations
    for(ts <- lexicalVariationEngine.lexicalVariations(tokens)) {
      matcher.add(ts)
    }
  }

  private def extractKBName(kb:String):String = {
    val slash = kb.lastIndexOf("/")
    val dot = kb.indexOf('.')
    val name = kb.substring(slash + 1, dot)
    name
  }

  /** Merges labels from src into dst, without overlapping any existing labels in dst */
  def mergeLabels(dst:Array[String], src:Array[String]) {
    assert(dst.length == src.length)

    var offset = 0
    while(offset < dst.length) {
      if(src(offset) != OUTSIDE_LABEL) {
        // no overlap allowed
        // if overlap is detected, the corresponding labels in src are discarded
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

  // Used by mergeLabels above
  private def overlap(dst:Array[String], src:Array[String], offset:Int):Boolean = {
    var position = offset
    if(dst(position) != OUTSIDE_LABEL) return true
    position += 1
    while(position < src.length && src(position).startsWith("I-")) {
      if(dst(position) != OUTSIDE_LABEL) return true
      position += 1
    }
    false
  }

  def scanText(words:Array[String], start:Int, end:Int):(Int, Int, Int, Int, Int) = {
    var letters = 0
    var digits = 0
    var upperCaseLetters = 0
    var characters = 0
    val spaces = words.length - 1
    for(offset <- start until end) {
      val word = words(offset)
      for (i <- word.indices) {
        val c = word.charAt(i)
        characters += 1
        if (Character.isLetter(c)) letters += 1
        if (Character.isUpperCase(c)) upperCaseLetters += 1
        if (Character.isDigit(c)) digits += 1
      }
    }
    (characters, letters, digits, upperCaseLetters, spaces)
  }

}

