package edu.arizona.sista.processors.bionlp.ner

import java.io.{InputStreamReader, BufferedReader}

import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.struct.HashTrie
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

import RuleNER._

/**
 * Rule-based NER for the Bio domain
 * User: mihais
 * Date: 5/11/15
 */
class RuleNER(val matchers:Array[(String, HashTrie)]) {

  def find(sentence:Sentence):Array[String] = {
    // findByPriority(sentence)
    findLongestMatch(sentence)
  }

  /**
   * Finds the longest match across all matchers.
   * This means that the longest match is always chosen, even if coming from a matcher with lower priority
   * Only ties are disambiguated according to the order provided in the constructor
   */
  def findLongestMatch(sentence:Sentence):Array[String] = {
    val words = sentence.words
    val caseInsensitiveWords = sentence.words.map(_.toLowerCase)

    var offset = 0
    val labels = new ArrayBuffer[String]()
    while(offset < words.length) {
      // stores the spans found by all matchers
      val spans = new Array[Int](matchers.length)

      // attempt to match each category at this offset
      for (i <- matchers.indices) {
        spans(i) = findAt(words, caseInsensitiveWords, matchers(i)._2, offset, sentence)
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
    // we only accept single tokens if they are tagged as NN*
    // see also removeSinglePrepositions
    if(end - start == 1 && ! sentence.tags.get(start).startsWith("NN"))
      return false

    true
  }

  /**
   * Inspects matchers in the order provided in the constructor
   * This means that a matcher with higher priority is preferred even if a longer one (with lower priority) exists!
   */
  def findByPriority(sentence:Sentence):Array[String] = {
    val overallLabels = new Array[String](sentence.size)
    for(i <- overallLabels.indices) overallLabels(i) = OUTSIDE_LABEL
    for(matcher <- matchers) {
      // the actual match
      var labels = matcher._2.find(sentence.words, matcher._1, OUTSIDE_LABEL)

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
  def load(kbs:List[String], caseInsensitive:Boolean = true):RuleNER = {
    logger.debug("Beginning to load the KBs for the rule-based bio NER...")
    val matchers = new ArrayBuffer[(String, HashTrie)]
    for(kb <- kbs) {
      val name = extractKBName(kb)
      val is = RuleNER.getClass.getClassLoader.getResourceAsStream(kb)
      assert(is != null, s"Failed to find KB file $kb in the classpath!")
      val reader = new BufferedReader(new InputStreamReader(is))
      val matcher = loadKB(reader, caseInsensitive)
      logger.debug(s"Loaded matcher for label $name. This matchers contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
      matchers += new Tuple2(name, matcher)
      reader.close()
    }
    logger.debug("KB loading completed.")
    new RuleNER(matchers.toArray)
  }

  def loadKB(reader:BufferedReader, caseInsensitive:Boolean): HashTrie = {
    val matcher = new HashTrie(caseInsensitive = caseInsensitive, internStrings = true)
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        val tokens = line.trim.split("\\s+")
        matcher.add(tokens)
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
}
