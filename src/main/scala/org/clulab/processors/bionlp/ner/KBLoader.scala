package org.clulab.processors.bionlp.ner

import java.io.{BufferedInputStream, InputStreamReader, BufferedReader}
import java.util.zip.GZIPInputStream

import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.struct.HashTrie
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Loads the KBs from bioresources under org/clulab/reach/kb/ner
  * These must be generated offline by KBGenerator; see bioresources/ner_kb.sh
  * User: mihais. 2/7/16.
  * Last Modified: Update for 5-column NER override file format and rename.
  */
object KBLoader {
  val logger = LoggerFactory.getLogger(classOf[BioNLPProcessor])

  val NAME_FIELD_NDX = 0                    // where the text of the NE is specified
  val LABEL_FIELD_NDX = 4                   // where the label of the NE is specified

  val RULE_NER_KBS = List( // knowledge for the rule-based NER; order is important: it indicates priority!
    "org/clulab/reach/kb/ner/Gene_or_gene_product.tsv.gz",
    "org/clulab/reach/kb/ner/Family.tsv.gz",
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

  val NER_OVERRIDE_KB =
    "org/clulab/reach/kb/NER-Grounding-Override.tsv.gz"

  /**
    * A horrible hack to keep track of entities that should not be labeled when in lower case, or upper initial
    */
  val ENTITY_STOPLIST = loadEntityStopList("org/clulab/reach/kb/ner_stoplist.txt")

  def loadEntityStopList(kb:String):Set[String] = {
    val stops = new mutable.HashSet[String]()
    val reader = loadStreamFromClasspath(kb)
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        val l = line.trim
        if(! l.isEmpty && ! l.startsWith("#")) {
          stops += l
        }
      }
    }
    reader.close()
    stops.toSet
  }

  def loadAll:RuleNER = {
    load(RULE_NER_KBS,
      Some(NER_OVERRIDE_KB), // allow overriding for some key entities
      useLemmas = false,
      caseInsensitive = true)
  }

  /**
    * Loads all KBs; KBs must be listed in descending order of their priorities
    */
  def load(
    kbs:List[String],
    overrideKB:Option[String],
    useLemmas:Boolean = false,
    caseInsensitive:Boolean = true):RuleNER = {

    logger.info("Beginning to load the KBs for the rule-based bio NER...")
    val matchers = new ArrayBuffer[(String, HashTrie)]
    val knownCaseInsensitives = new mutable.HashSet[String]()

    // load the override KBs first, so they take priority during matching
    overrideKB.foreach(okb => {
      val reader = loadStreamFromClasspath(okb)
      val overrideMatchers = loadOverrideKB(reader, caseInsensitive, knownCaseInsensitives)
      for(name <- overrideMatchers.keySet.toList.sorted) {
        val matcher = overrideMatchers.get(name).get
        logger.info(s"Loaded OVERRIDE matcher for label $name. This matcher contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
        matchers += new Tuple2(name, matcher)
      }
      reader.close()
    })

    // load the standard KBs
    for(kb <- kbs) {
      val name = extractKBName(kb)
      val reader = loadStreamFromClasspath(kb)
      val matcher = loadKB(reader, caseInsensitive, knownCaseInsensitives)
      logger.info(s"Loaded matcher for label $name. This matcher contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
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

  private def loadOverrideKB(
    reader:BufferedReader,
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): Map[String, HashTrie] = {
    val matchers = new mutable.HashMap[String, HashTrie]()
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        addOverrideLine(line, matchers, caseInsensitive, knownCaseInsensitives)
      }
    }
    matchers.toMap
  }

  private def addOverrideLine(
    inputLine:String,
    matchers:mutable.HashMap[String, HashTrie],
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): Unit = {
    val line = inputLine.trim
    if(! line.startsWith("#")) { // skip comments starting with #
      val blocks = line.split("\t")
      val entity = blocks(NAME_FIELD_NDX)   // grab the text of the named entity
      val label = blocks(LABEL_FIELD_NDX)   // grab the label of the named entity

      val tokens = entity.split("\\s+")
      if(tokens.length == 1 && line.toLowerCase == line) { // keep track of all lower case ents that are single letter
        knownCaseInsensitives.add(line)
      }
      val matcher = matchers.getOrElseUpdate(label,
        new HashTrie(caseInsensitive = caseInsensitive, internStrings = true))
      matcher.add(tokens)
    }
  }

  private def addLine(inputLine:String, matcher:HashTrie, knownCaseInsensitives:mutable.HashSet[String]): Unit = {
    val line = inputLine.trim
    if(! line.startsWith("#")) {
      val tokens = line.split("\\s+")
      matcher.add(tokens)
      if(tokens.length == 1 && line.toLowerCase == line) { // keep track of all lower case ents that are single letter
        knownCaseInsensitives.add(line)
      }
    }
  }

  private def loadKB(
    reader:BufferedReader,
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): HashTrie = {
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
