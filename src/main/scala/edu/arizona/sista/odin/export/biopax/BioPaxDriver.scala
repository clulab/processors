package edu.arizona.sista.odin.export.biopax

import java.io._

import edu.arizona.sista.processors.{DocumentSerializer, Document}
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.odin._
import edu.arizona.sista.odin.domains.bigmechanism.dryrun2015.{DarpaActions,Ruler}

import org.slf4j.LoggerFactory

/**
  * Top-level test driver for BioPax output development.
  *   Author: by Tom Hicks, after program by Gus Hahn-Powell.
  *   Last Modified: Print string mentions for events only.
  */
object BioPaxDriver extends App {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  val entityRules = Ruler.readEntityRules()
  val eventRules = Ruler.readEventRules()
  val rules = entityRules + "\n\n" + eventRules

  val ds = new DocumentSerializer
  val actions = new DarpaActions
  val processor = new BioNLPProcessor()
  val extractor = new Ruler(rules, actions)

  val PapersDir = s"${System.getProperty("user.dir")}/src/test/resources/papers/"
  val paperNames = Seq(
    "MEKinhibition.txt.ser",
    "UbiquitinationofRas.txt.ser",
    "PMC3441633.txt.ser",
    "PMC3847091.txt.ser"
  )

  def cleanText (m: Mention): String = {
    """(\s+|\n|\t|[;])""".r.replaceAllIn(m.document.sentences(m.sentence).getSentenceText(), " ")
  }

  def docFromSerializedFile (filename: String): Document = {
    val br = new BufferedReader(new FileReader(filename))
    val doc = ds.load(br)
    doc
  }

  def getText(fileName: String):String = scala.io.Source.fromFile(fileName).mkString

  // val outDir = s"${System.getProperty("java.io.tmpdir")}" + File.separator
  val outDir = s"${System.getProperty("user.dir")}" + File.separator
  def mkOutputName (paper:String, ext:String): String = {
    outDir + {"""^.*?/|.txt.ser""".r.replaceAllIn(paper, "")} + ext
  }

  def processPapers (papers:Seq[String], asStrings:Boolean=false): Unit = {
    papers.foreach { paper => processPaper(paper, asStrings) }
  }

  def processPaper (paper: String, asStrings:Boolean=false): Unit = {
    val ext = if (asStrings) ".txt" else ".rdf"
    val outName = mkOutputName(paper, ext)
    val outFile = new FileOutputStream(new File(outName))
    val inFile = s"$PapersDir/$paper"

    val doc = paper match {
      case ser if ser.endsWith("ser") => docFromSerializedFile(inFile)
      case _ => processor.annotate(getText(inFile))
    }

    val mentions = extractor.extractFrom(doc)
    val sortedMentions = mentions.sortBy(m => (m.sentence, m.start)) // sort by sentence, start idx
    if (asStrings)
      outputEventMentions(sortedMentions, doc, outFile)
    else
      outputBioPax(sortedMentions, doc, outFile)
  }

  /** Output string representations for the given sequence of mentions. */
  def outputAllMentions (mentions:Seq[Mention], doc:Document, fos:FileOutputStream): Unit = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(fos)))
    val paxer:BioPaxer = new BioPaxer()
    mentions.foreach { m =>
      paxer.mentionToStrings(m).foreach { str => out.println(str) }
    }
    out.flush()
    out.close()
  }

  /** Output a BioPax RDF representation for the given sequence of mentions. */
  def outputBioPax (mentions:Seq[Mention], doc:Document, out:FileOutputStream): Unit = {
    val paxer:BioPaxer = new BioPaxer()
    val model = paxer.buildModel(mentions, doc)
    if (!model.getObjects().isEmpty())
      paxer.outputModel(model, out)
    out.flush()
    out.close()
  }

  /** Output string representations for event mentions in the given sequence. */
  def outputEventMentions (mentions:Seq[Mention], doc:Document, fos:FileOutputStream): Unit = {
    val paxer:BioPaxer = new BioPaxer()
    paxer.outputFilteredMentions("Event", mentions, doc, fos)
    fos.close()                             // NOT NEEDED? REMOVE?
  }


  // Top-level Main of script:
  var asStrings:Boolean = false
  if (!args.isEmpty) asStrings = true
  processPapers(paperNames, asStrings)

}
