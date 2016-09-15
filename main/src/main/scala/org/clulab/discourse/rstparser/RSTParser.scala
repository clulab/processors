package org.clulab.discourse.rstparser

import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import scala.io.StdIn
import org.clulab.discourse.rstparser.Utils._
import org.clulab.processors.{Processor, Document}
import org.slf4j.LoggerFactory
import scala.collection.mutable.ArrayBuffer
import org.clulab.utils.Files
import org.clulab.discourse.rstparser.RSTParser._

/**
 * The main entry point: RST parser that parses arbitrary input text
 * User: mihais
 * Date: 5/26/14
 */
class RSTParser {
  var corpusStats:CorpusStats = null
  var eduModel:EDUClassifier = null
  var structModel:StructureClassifier = null
  var relModel:RelationClassifier = null

  def saveTo(path:String) {
    val writer = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(path)), Files.FILE_CHARSET)))
    corpusStats.saveTo(writer)
    eduModel.saveTo(writer)
    structModel.saveTo(writer, saveCorpusStats = false)
    relModel.saveTo(writer, saveCorpusStats = false)
    writer.close()
  }

  def train(trees: List[(DiscourseTree, Document)], cs: CorpusStats, dependencySyntax: Boolean) {
    corpusStats = cs

    logger.debug("Training the EDU model...")
    eduModel = new EDUClassifier
    eduModel.train(trees, corpusStats)

    logger.debug("Training the RELATION model...")
    var prefixes = RelationClassifier.CONSTITUENTSYNTAX_PREFIXES
    if(dependencySyntax) prefixes = RelationClassifier.DEPENDENCYSYNTAX_PREFIXES
    relModel = new RelationClassifier(prefixes, withNuclearity = true)
    relModel.train(trees, corpusStats)

    logger.debug("Training the STRUCT model...")
    structModel = new StructureClassifier
    structModel.train(trees, corpusStats)
  }

  def test(trees: List[(DiscourseTree, Document)]) {
    logger.debug("Started parsing...")
    val scorer = new DiscourseScorer
    val structScoreGold = new DiscourseScore()
    val fullScoreGold = new DiscourseScore()
    val structScorePred = new DiscourseScore()
    val fullScorePred = new DiscourseScore()

    for(td <- trees) {
      val sys = parseWithGoldEDUs(td._2, td._1)
      scorer.score(sys, td._1, structScoreGold, ScoreType.OnlyStructure)
      scorer.score(sys, td._1, fullScoreGold, ScoreType.Full)
    }

    var inc = 0
    var total = 0
    for(td <- trees) {
      val sys = parse(td._2)._1
      scorer.score(sys, td._1, structScorePred, ScoreType.OnlyStructure)
      scorer.score(sys, td._1, fullScorePred, ScoreType.Full)

      val (i, t) = CheckSameSentence.checkTree(sys, td._2)
      inc += i
      total += t
    }

    logger.debug(s"Done parsing ${trees.size} documents.")
    logger.debug(s"Found $inc inconsistencies out of $total attachments.")
    logger.info("STRUCT SCORE (with gold EDUs):\n" + structScoreGold)
    logger.info("FULL SCORE (with gold EDUs):\n" + fullScoreGold)
    logger.info("STRUCT SCORE (with predicted EDUs):\n" + structScorePred)
    logger.info("FULL SCORE (with predicted EDUs):\n" + fullScorePred)
  }

  def parseWithGoldEDUs(doc:Document, tree:DiscourseTree):DiscourseTree = {
    val edus = mkGoldEDUs(tree, doc)
    val sysTree = structModel.mkTree(edus, doc, relModel)
    sysTree
  }

  def parse(doc:Document,
            justEdus:Boolean = false,
            verbose:Boolean = false):(DiscourseTree, Array[Array[(Int, Int)]]) = {
    val edus = predictEDUs(doc)

    if(verbose) {
      println("Found EDUs:")
      for(i <- edus.indices) {
        print(s"\tS$i:")
        for(edu <- edus(i)) {
          print(s" (${edu._1}, ${edu._2})")
        }
        println()
      }
    }

    val tree = justEdus match {
      case true => mkTreeFromEDUs(edus, doc)
      case _ => structModel.mkTree(edus, doc, relModel, verbose)
    }

    (tree, edus)
  }

  def mkTreeFromEDUs(edus: Array[Array[(Int, Int)]], doc:Document): DiscourseTree = {
    val trees = structModel.edusToTerminalTrees(doc, edus, 0, edus.length)
    new DiscourseTree("edus", RelationDirection.None, trees)
  }

  def predictEDUs(doc:Document):Array[Array[(Int, Int)]] = {
    // these are used during feature generation
    val connectiveBIOs = ConnectiveMatcher.matchConnectives(doc)

    val eduBuffer = new Array[ArrayBuffer[(Int, Int)]](doc.sentences.length)
    for(si <- doc.sentences.indices) {
      eduBuffer(si) = new ArrayBuffer[(Int, Int)]()
      val sent = doc.sentences(si)
      var start = 0
      for(ti <- 1 until sent.size - 1) {
        val token = new EDUToken(new TokenOffset(si, ti), doc, connectiveBIOs, false)
        val datum = eduModel.mkDatum(token)
        val l = eduModel.classOf(datum)
        if(l == EDUClassifier.POS) {
          // found the beginning of an EDU
          eduBuffer(si) += new Tuple2(start, ti - 1)
          start = ti
        }
      }
      eduBuffer(si) += new Tuple2(start, sent.size - 1)
    }

    val edus = new Array[Array[(Int, Int)]](eduBuffer.length)
    for(i <- eduBuffer.indices) {
      edus(i) = eduBuffer(i).toArray
    }
    edus
  }
}

object RSTParser {
  val logger = LoggerFactory.getLogger(classOf[RSTParser])

  val DEFAULT_CONSTITUENTSYNTAX_MODEL_PATH = "org/clulab/discourse/rstparser/model.const.rst.gz"
  val DEFAULT_DEPENDENCYSYNTAX_MODEL_PATH = "org/clulab/discourse/rstparser/model.dep.rst.gz"

  def loadFrom(path:String):RSTParser = {
    logger.debug("Loading RST parsing model from: " + path)
    val parser = new RSTParser
    val is = RSTParser.getClass.getClassLoader.getResourceAsStream(path)
    assert(is != null, s"Failed to find model file $path in the classpath!")
    val reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(is)))
    val corpusStats = CorpusStats.loadFrom[String](reader)
    val em = EDUClassifier.loadFrom(reader)
    val sm = StructureClassifier.loadFrom(reader, corpusStats)
    val rm = RelationClassifier.loadFrom(reader, corpusStats)
    reader.close()

    parser.eduModel = em
    parser.relModel = rm
    parser.structModel = sm

    logger.debug("Done loading model.")
    parser
  }

  def shell(parser:RSTParser, proc:Processor) {
    while(true) {
      print("> ")
      val line = StdIn.readLine()
      if(line != null && line.trim.length > 0) {
        val doc = proc.annotate(line)
        val tree = parser.parse(doc)._1
        println("Discourse tree:")
        println(tree)
      }
    }
  }
}
