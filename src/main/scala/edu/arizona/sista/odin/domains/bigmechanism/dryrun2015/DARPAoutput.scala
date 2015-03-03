package edu.arizona.sista.odin.domains.bigmechanism.dryrun2015

import java.io.{BufferedReader, File, FileReader, PrintWriter}

import edu.arizona.sista.odin.{Mention, EventMention}
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.processors.{Document, DocumentSerializer}
import org.slf4j.LoggerFactory

/**
 * Created by gus on 12/22/14.
 */

object DARPAoutput extends App {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  val entityRules = Ruler.readEntityRules()
  val eventRules = Ruler.readEventRules()
  val rules = entityRules + "\n\n" + eventRules

  val ds = new DocumentSerializer

  val actions = new DarpaActions

  val proc = new BioNLPProcessor()
  val extractor = new Ruler(rules, actions)

  //def processPapers(papers: Seq[String]) = papers.foreach { paper => processPaper(paper)}

  def getText(fileName: String):String = scala.io.Source.fromFile(fileName).mkString
  def processPaper(paper: String, outFile: String): Unit = {

    val output = new PrintWriter(new File(outFile))

    val header = s"Mention Count;Relation;Model Link (BioPax or BEL);‘English-like’ Description;Model Representation;Source Text\n"

    println(s"Writing output to $outFile")
    output.write(header)

    // are we dealing with a serialized file or not?
    val doc = paper match {
      // is it a serialized file?
      case ser if ser.endsWith("ser") => docFromSerializedFile(ser)
      // assume it needs to be annotated...
      case _ => proc.annotate(getText(paper))
    }

    val mentions: Map[String, Seq[EventMention]] =
         retrieveMentions(doc)
        .groupBy(m => m.repr)

    mentions.foreach(pair => writeEvents(pair._1, pair._2, output))

    output.close()
  }

  def docFromSerializedFile(filename: String): Document = {
    val br = new BufferedReader(new FileReader(filename))
    val doc = ds.load(br)
    doc
  }

  def retrieveMentions(doc: Document): Seq[EventMention] = {
    extractor.extractFrom(doc).filter(_.isInstanceOf[EventMention]).map(_.asInstanceOf[EventMention])
  }

  def cleanText(m: Mention): String = {
    """(\s+|\n|\t|[;])""".r.replaceAllIn(m.document.sentences(m.sentence).getSentenceText(), " ")
  }

  def writeEvents(representation: String, mentions: Seq[EventMention], output: PrintWriter) {
    def getText: String = {
      mentions.sortBy(m => (m.sentence, m.start)) // sort by sentence, start idx
        .map(m => cleanText(m)) // get text
        .mkString("  ")
    }
    output.write(s"${mentions.size};;;;$representation;$getText\n")
  }

  def expandPath(fileName:String):String = """^~""".r.replaceFirstIn(fileName, System.getProperty("user.home"))

  val inFile = expandPath(args.head)
  val outFile = expandPath(args.last)

  logger.info(s"Reading:\t$inFile")
  logger.info(s"Generating:\t$outFile")

  processPaper(inFile, outFile)
}
