package edu.arizona.sista.discourse.rstparser

import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import scala.io.StdIn
import edu.arizona.sista.discourse.rstparser.Utils._
import edu.arizona.sista.processors.{Processor, Document}
import org.slf4j.LoggerFactory
import scala.collection.mutable.ArrayBuffer
import edu.arizona.sista.utils.{Files, StringUtils}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.discourse.rstparser.RSTParser._

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

  def train(trainDirName:String, dependencySyntax:Boolean) {
    val (trees, cs) = mkTrees(trainDirName, CacheReader.getProcessor(dependencySyntax))
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

  def test(testDirName:String, dependencySyntax:Boolean) {
    val (trees, _) = mkTrees(testDirName, CacheReader.getProcessor(dependencySyntax), makeStats = false)

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
            verbose:Boolean = false):(DiscourseTree, Array[Array[(Int, Int)]]) = {
    val edus = predictEDUs(doc)

    if(verbose) {
      println("Found EDUs:")
      for(i <- 0 until edus.size) {
        print(s"\tS$i:")
        for(edu <- edus(i)) {
          print(s" (${edu._1}, ${edu._2})")
        }
        println()
      }
    }

    val tree = structModel.mkTree(edus, doc, relModel, verbose)
    (tree, edus)
  }

  def predictEDUs(doc:Document):Array[Array[(Int, Int)]] = {
    // these are used during feature generation
    val connectiveBIOs = ConnectiveMatcher.matchConnectives(doc)

    val eduBuffer = new Array[ArrayBuffer[(Int, Int)]](doc.sentences.size)
    for(si <- 0 until doc.sentences.size) {
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

    val edus = new Array[Array[(Int, Int)]](eduBuffer.size)
    for(i <- 0 until eduBuffer.size) {
      edus(i) = eduBuffer(i).toArray
    }
    edus
  }
}

object RSTParser {
  val logger = LoggerFactory.getLogger(classOf[RSTParser])

  val DEFAULT_CONSTITUENTSYNTAX_MODEL_PATH = "edu/arizona/sista/discourse/rstparser/model.const.rst.gz"
  val DEFAULT_DEPENDENCYSYNTAX_MODEL_PATH = "edu/arizona/sista/discourse/rstparser/model.dep.rst.gz"

  def mkTrees(path:String,
              processor:Processor,
              makeStats:Boolean = true): (List[(DiscourseTree, Document)], CorpusStats) = {
    logger.debug("Loading training trees...")
    val trees = CacheReader.load(path, processor)

    if(makeStats) {
      logger.debug("Counting words...")
      val knownWords = countWords(trees)
      logger.debug("Counting prefix/suffix n-grams...")
      val knownNgrams = countNgrams(trees)
      val corpusStats = new CorpusStats(knownWords, knownNgrams)

      (trees, corpusStats)
    } else {
      (trees, null)
    }
  }

  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)
    var parser:RSTParser = null

    if(props.containsKey("train")) {
      parser = new RSTParser
      parser.train(props.getProperty("train"), props.containsKey("dep"))
      if(props.containsKey("model")) {
        parser.saveTo(props.getProperty("model"))
      }
    }
    if (props.containsKey("test")) {
      val defaultModelPath = RSTParser.DEFAULT_CONSTITUENTSYNTAX_MODEL_PATH
      val modelPath = if (props.containsKey("model")) props.getProperty("model") else defaultModelPath
      parser = loadFrom(modelPath)
      parser.test(props.getProperty("test"), props.containsKey("dep"))
    }
    if(props.containsKey("shell")) {
      var path = RSTParser.DEFAULT_CONSTITUENTSYNTAX_MODEL_PATH
      if(props.containsKey("dep")) path = RSTParser.DEFAULT_DEPENDENCYSYNTAX_MODEL_PATH

      if(parser == null && props.containsKey("model")) {
        parser = RSTParser.loadFrom(props.getProperty("model", path))
      } else {
        throw new RuntimeException("ERROR: property \"model\" or \"train\" must be specified!")
      }
      val proc = new CoreNLPProcessor()
      shell(parser, proc)
    }
  }

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
