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
    val overallLabels = new Array[String](sentence.size)
    for(i <- 0 until overallLabels.length) overallLabels(i) = OUTSIDE_LABEL
    for(matcher <- matchers) {
      val labels = matcher._2.find(sentence.words, matcher._1, OUTSIDE_LABEL)
      // matchers must be stored in descending order of their priorities
      // so we do not allow new labels to overwrite labels already generated
      RuleNER.mergeLabels(overallLabels, labels)
    }
    overallLabels
  }
}

object RuleNER {
  val logger = LoggerFactory.getLogger(classOf[RuleNER])
  val OUTSIDE_LABEL = "O"

  /** Loads all KBs; KBs must be listed in descending order of their priorities, e.g., protein families before Uniprot */
  def load(kbs:List[String], caseInsensitive:Boolean = true):RuleNER = {
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
