package org.clulab.processors.bionlp.ner

import java.io._
import java.text.SimpleDateFormat
import java.util.Date
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import org.clulab.processors.bionlp.BioNLPProcessor
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation
import edu.stanford.nlp.pipeline.Annotation
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import scala.collection.JavaConverters._

case class KBEntry(kbName:String, neLabel:String, validSpecies:Set[String])

/**
  *
  * User: mihais
  * Date: 2/7/16
  * Last Modified: Update for 5-column NER override file format.
  */
object KBGenerator {
  val logger = LoggerFactory.getLogger(classOf[BioNLPProcessor])

  val NAME_FIELD_NDX = 0                    // config column containing the KB name
  val LABEL_FIELD_NDX = 1                   // config column containing the type label
  val SPECIES_FIELD_NDX = 2                 // KB column containing the species of the name entity

  /** Minimal processor, used solely for the tokenization of resources */
  lazy val processor = new BioNLPProcessor(
    withCRFNER = false,
    withContext = false,
    withRuleNER = false,
    withDiscourse = ShallowNLPProcessor.NO_DISCOURSE)

  def main (args: Array[String]) {
    val configFile = args(0)
    val inputDir = args(1)
    val outputDir = args(2)

    val entries = loadConfig(configFile)
    logger.info(s"Will convert a total of ${entries.size} KBs:")
    for(entry <- entries) {
      logger.info(s"KB:${entry.kbName} to NE:${entry.neLabel} using species:${entry.validSpecies}.")
      // delete the old output
      val f = new File(mkOutputFile(entry, outputDir))
      if(f.exists()) {
        f.delete()
        logger.info(s"Deleted old output ${f.getAbsolutePath}.")
      }
    }

    for(entry <- entries) {
      convertKB(entry, inputDir, outputDir)
    }
  }

  def loadConfig(configFile:String):Seq[KBEntry] = {
    val entries = new ListBuffer[KBEntry]
    for(line <- io.Source.fromFile(configFile).getLines()) {
      val trimmedLine = line.trim
      if(! trimmedLine.isEmpty && ! trimmedLine.startsWith("#")) {
        val tokens = trimmedLine.split("\t")
        assert(tokens.length >= 2)
        val kbName = tokens(NAME_FIELD_NDX)
        val neLabel = tokens(LABEL_FIELD_NDX)
        val species = new mutable.HashSet[String]()
        for(i <- 2 until tokens.length) {
          species += tokens(i)
        }
        entries += KBEntry(kbName, neLabel, species.toSet)
      }
    }
    entries.toList
  }

  def mkOutputFile(entry:KBEntry, outputDir:String):String =
    outputDir + File.separator + entry.neLabel + ".tsv.gz"

  def convertKB(entry:KBEntry, inputDir:String, outputDir:String): Unit = {
    logger.info(s"Converting ${entry.kbName}...")
    val inputPath = inputDir + File.separator + entry.kbName + ".tsv.gz"
    val b = new BufferedReader(
      new InputStreamReader(
        new GZIPInputStream(
          new FileInputStream(inputPath))))

    var done = false
    var lineCount = 0
    val outputLines = new ArrayBuffer[String]()
    while(! done) {
      val line = b.readLine()
      if(line == null) {
        done = true
      } else {
        val trimmedLine = line.trim
        if(! trimmedLine.isEmpty && ! trimmedLine.startsWith("#")) { // skip comments
          val kbTokens = line.split("\t")
          if(containsValidSpecies(entry, kbTokens)) { // this is a protein from a species we want
            lineCount += 1
            val ne = kbTokens(0) // we enforce that the first token is the actual NE to be considered
            val tokens = tokenizeResourceLine(ne) // tokenize using BioNLPProcessor
            outputLines += tokens.mkString(" ")
          }
        }
      }
    }
    b.close()

    // append to output; we may have multiple KBs using the same NE!
    val first = ! new File(mkOutputFile(entry, outputDir)).exists()
    val ow =
      new PrintWriter(
        new GZIPOutputStream(
          new FileOutputStream(mkOutputFile(entry, outputDir), true)))
    if(first) ow.println(s"# Created by ${getClass.getName} on $now.")
    val uniqLines = outputLines.toSeq
      .filter(_.nonEmpty)
      .sorted
      .distinct
    ow.print(uniqLines.mkString("\n"))
    ow.println()
    ow.close()

    logger.info(s"Done. Read $lineCount lines (${uniqLines.size} distinct) from ${entry.kbName}")
  }

  def now:String = {
    val dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
    val date = new Date()
    dateFormat.format(date)
  }

  /**
    * Tokenizes a resource line with BioNLPProcessor
    * This is important! We must guarantee that KB text is processed similarly to raw text!
 *
    * @param line The KB line
    * @return The tokenized line
    */
  def tokenizeResourceLine(line:String):Array[String] = {
    val annotation = new Annotation(processor.preprocessText(line)) // preprocess text, e.g., replace Unicode with ASCII
    processor.tokenizerWithoutSentenceSplitting.annotate(annotation) // tokenization
    val origTokens = annotation.get(classOf[TokensAnnotation]).asScala.toArray
    processor.postprocessTokens(origTokens).map(_.word()) // tokenization post-processing

    // old doc, with unnecessary overhead
    //val doc = mkDocument(preprocessText(line), keepText = false)
    //doc.sentences.flatMap(_.words)
  }

  def containsValidSpecies(entry:KBEntry, tokens:Array[String]):Boolean = {
    if(entry.validSpecies.isEmpty)
      return true

    // if mentioned, the species must be a token at the correct zero-indexed position.
    if(entry.validSpecies.contains(tokens(SPECIES_FIELD_NDX)))
      return true

    false
  }
}
