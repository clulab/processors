package org.clulab.discourse.rstparser

import java.io.{InputStreamReader, BufferedReader}
import java.util.regex.Pattern
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import org.clulab.processors.{Sentence, Document}

/**
 * Matches a set of known connectives in a given document
 * User: mihais
 * Date: 5/21/14
 * Last Modified: Fix compiler issue: import scala.io.Source.
 */
class ConnectiveMatcher

object ConnectiveMatcher {
  val logger = LoggerFactory.getLogger(classOf[ConnectiveMatcher])

  val DEFAULT_CONNECTIVE_FILE = "org/clulab/discourse/rstparser/discourse_connectives.txt"

  lazy val CONNECTIVES = loadConnectives(DEFAULT_CONNECTIVE_FILE)

  val NO_CONNECTIVE = "O"
  val BEGIN_CONNECTIVE = "B"
  val INSIDE_CONNECTIVE = "I"

  def loadConnectives(path:String):Iterable[Array[String]] = {
    val is = RSTParser.getClass.getClassLoader.getResourceAsStream(path)
    val lines = Source.fromInputStream(is).getLines().toList.sortWith(desc)
    //println(lines)
    logger.debug("Loaded " + lines.length + " discourse connectives.")
    is.close()

    val connectives = new ArrayBuffer[Array[String]]()
    for(line <- lines) {
      val l = line.trim
      if(l.length > 0) {
        connectives += l.toLowerCase.split("\\s+")
      }
    }

    connectives.toList
  }

  def desc(s1:String, s2:String):Boolean = {
    if(countSpaces(s1) > countSpaces(s2))
      return true
    false
  }

  def countSpaces(s:String):Int = {
    val SPACE_PATTERN = Pattern.compile("\\s+", Pattern.CASE_INSENSITIVE)
    val m = SPACE_PATTERN.matcher(s)
    var offset = 0
    var count = 0
    while(m.find(offset)) {
      count += 1
      offset = m.end()
    }
    //println(count + "\t" + s)
    count
  }

  /** Matches known connectives in the document, producing an array of BIO labels */
  def matchConnectives(doc:Document):Array[Array[String]] = {
    val sents = new ArrayBuffer[Array[String]]()
    for(s <- doc.sentences) {
      sents += matchConnectivesInSentence(s)
    }
    sents.toArray
  }

  def matchConnectivesInSentence(sent:Sentence):Array[String] = {
    val cs = new Array[String](sent.size)
    for(i <- 0 until cs.length)
      cs(i) = NO_CONNECTIVE
    var offset = 0
    while(offset < cs.length) {
      val conn = matchesConnective(sent.words, offset)
      if(conn != null) {
        val len = conn.length
        cs(offset) = BEGIN_CONNECTIVE
        for(i <- 1 until len)
          cs(offset + i) = INSIDE_CONNECTIVE
        offset += len
      } else {
        offset += 1
      }
    }
    cs
  }

  def matchesConnective(words:Array[String], offset:Int):Array[String] = {
    for(pattern <- CONNECTIVES) {
      var found = true
      //logger.debug("Searching for connective: " + pattern.mkString(" "))
      for(i <- 0 until pattern.length; if found) {
        if(i + offset >= words.size) found = false
        else if(!pattern(i).equalsIgnoreCase(words(i + offset))) {
          found = false
        }
      }
      if(found) {
        //logger.debug("Matched connective at " + offset + ": " + sent.words.mkString(" "))
        return pattern
      }
    }
    null
  }
}
