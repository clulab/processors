package edu.arizona.sista.processors.bionlp.ner

import java.io.{BufferedInputStream, InputStreamReader, BufferedReader}
import java.util.zip.GZIPInputStream

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.struct.HashTrie
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Loads the KBs from bioresources under org/clulab/reach/kb/ner
  * These must be generated offline by KBGenerator; see bioresources/ner_kb.sh
  * User: mihais. 2/7/16.
  * Last Modified: Update KB load order to match ner_kb.conf, add tissue type file.
  */
object KBLoader {
  val logger = LoggerFactory.getLogger(classOf[BioNLPProcessor])

  val RULE_NER_KBS = List( // knowledge for the rule-based NER; order is important: it indicates priority!
    "org/clulab/reach/kb/ner/Family.tsv.gz",
    "org/clulab/reach/kb/ner/Gene_or_gene_product.tsv.gz",
    "org/clulab/reach/kb/ner/Cellular_component.tsv.gz",
    "org/clulab/reach/kb/ner/Simple_chemical.tsv.gz",
    "org/clulab/reach/kb/ner/Site.tsv.gz",
    "org/clulab/reach/kb/ner/BioProcess.tsv.gz",
    "org/clulab/reach/kb/ner/Species.tsv.gz",
    "org/clulab/reach/kb/ner/CellLine.tsv.gz",
    "org/clulab/reach/kb/ner/TissueType.tsv.gz",
    "org/clulab/reach/kb/ner/CellType.tsv.gz",
    "org/clulab/reach/kb/ner/Organ.tsv.gz"
  )

  def loadAll:RuleNER = {
    load(RULE_NER_KBS, useLemmas = false, caseInsensitive = true)
  }

  /**
    * Loads all KBs; KBs must be listed in descending order of their priorities
    */
  def load(kbs:List[String], useLemmas:Boolean = false, caseInsensitive:Boolean = true):RuleNER = {
    logger.info("Beginning to load the KBs for the rule-based bio NER...")
    val matchers = new ArrayBuffer[(String, HashTrie)]
    val knownCaseInsensitives = new mutable.HashSet[String]()
    for(kb <- kbs) {
      val name = extractKBName(kb)
      val reader = loadStreamFromClasspath(kb)
      val matcher = loadKB(reader, caseInsensitive, knownCaseInsensitives)
      logger.info(s"Loaded matcher for label $name. This matchers contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
      matchers += new Tuple2(name, matcher)
      reader.close()
    }
    logger.info("KB loading completed.")
    new RuleNER(matchers.toArray, knownCaseInsensitives.toSet, useLemmas)
  }

  private def loadStreamFromClasspath(path: String):BufferedReader = {
    val is = getClass.getClassLoader.getResourceAsStream(path)
    if (is == null) throw new RuntimeException(s"ERROR: cannot find resource $path in classpath!")

    if (path.endsWith(".gz"))
      new BufferedReader(
        new InputStreamReader(
          new GZIPInputStream(new BufferedInputStream(is))))
    else
      new BufferedReader(
        new InputStreamReader(
          new BufferedInputStream(is)))
  }

  private def addLine(inputLine:String, matcher:HashTrie, knownCaseInsensitives:mutable.HashSet[String]): Unit = {
    val line = inputLine.trim
    if(! line.startsWith("#")) {
      val tokens = line.split("\\s+")
      matcher.add(tokens)
      if(tokens.length == 1 && line.toLowerCase == line) {
        knownCaseInsensitives.add(line)
      }
    }
  }

  private def loadKB(reader:BufferedReader, caseInsensitive:Boolean, knownCaseInsensitives:mutable.HashSet[String]): HashTrie = {
    val matcher = new HashTrie(caseInsensitive = caseInsensitive, internStrings = true)
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        addLine(line, matcher, knownCaseInsensitives)
      }
    }
    matcher
  }

  private def extractKBName(kb:String):String = {
    val slash = kb.lastIndexOf("/")
    val dot = kb.indexOf('.')
    val name = kb.substring(slash + 1, dot)
    name
  }
}
