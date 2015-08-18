package edu.arizona.sista.processors.fastnlp

import java.util.Properties

import edu.arizona.sista.discourse.rstparser.RSTParser
import edu.arizona.sista.processors.corenlp.CoreNLPUtils
import edu.arizona.sista.processors.shallownlp.ShallowNLPProcessor
import edu.arizona.sista.processors.{DependencyMap, Sentence, Document}
import edu.arizona.sista.struct.DirectedGraph
import edu.arizona.sista.utils.Files
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.parser.nndep.DependencyParser
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.semgraph.SemanticGraphFactory
import edu.stanford.nlp.trees.GrammaticalStructure
import org.maltparserx.MaltParserService
import FastNLPProcessor._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.collection.mutable
import org.maltparserx
import scala.collection.JavaConversions._
import edu.arizona.sista.discourse.rstparser.Utils._

/**
 * Fast NLP tools
 * Extends ShallowNLP with a dependency parser based on maltparser or the Stanford NN dependency parser
 * This means that constituent trees and coreference, which depends on that, are not available
 * The default setting is to use the Stanford parser with "basic" dependencies
 * Malt produces ONLY Stanford "basic" dependencies, rather than "collapsed" ones
 * User: mihais
 * Date: 1/4/14
 */
class FastNLPProcessor(internStrings:Boolean = true,
                       useMalt:Boolean = false, // if false it uses the new Stanford dependency parser
                       withDiscourse:Boolean = false)
  extends ShallowNLPProcessor(internStrings) {

  /**
   * One maltparser instance for each thread
   * MUST have one separate malt instance per thread!
   * malt uses a working directory which is written at runtime
   * using ThreadLocal variables guarantees that each thread gets its own working directory
   */
  lazy val maltService = new ThreadLocal[MaltParserService]

  /** RST discourse parser using only dependency based syntax */
  lazy val rstDependencyParser = fetchRSTParser(RSTParser.DEFAULT_DEPENDENCYSYNTAX_MODEL_PATH)

  /** Stanford's NN dependency parser */
  lazy val stanfordDepParser = fetchStanfordParser()

  override def parse(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return
    if (doc.sentences.head.tags.isEmpty)
      throw new RuntimeException("ERROR: you have to run the POS tagger before NER!")
    if (doc.sentences.head.lemmas.isEmpty)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before NER!")

    if (useMalt) {
      // use the malt parser
      parseWithMalt(doc)
    } else {
      parseWithStanford(doc, annotation.get)
    }
  }

  private def parseWithMalt(doc:Document) {
    // parse each individual sentence
    val debug = false
    for (sentence <- doc.sentences) {
      if (debug) {
        print("PARSING SENTENCE:")
        for (i <- 0 until sentence.size) print(" " + sentence.words(i))
        println()
      }
      val dg = parseSentence(sentence)
      // Note: malt only support basic Stanford dependencies!
      sentence.setDependencies(DependencyMap.STANFORD_BASIC, dg)
      if (debug) {
        println("DONE.")
      }
    }
  }

  private def parseWithStanford(doc:Document, annotation:Annotation) {
    val sas = annotation.get(classOf[SentencesAnnotation])
    var offset = 0
    for (sa <- sas) {
      // convert parens to Penn Treebank symbols because this is what the parser has seen in training
      val words = CoreNLPUtils.parensToSymbols(sa.get(classOf[CoreAnnotations.TokensAnnotation]))
      sa.set(classOf[CoreAnnotations.TokensAnnotation], words)

      // println("Parsing sentence: " + words.map(_.word()).mkString(" "))

      // the actual parsing job
      val gs = stanfordDepParser.predict(sa)

      // convert to Stanford's semantic graph representation
      val basicDeps = SemanticGraphFactory.makeFromTree(gs, SemanticGraphFactory.Mode.BASIC, GrammaticalStructure.Extras.NONE, true, null)
      val collapsedDeps = SemanticGraphFactory.makeFromTree(gs, SemanticGraphFactory.Mode.CCPROCESSED, GrammaticalStructure.Extras.NONE, true, null)

      // convert to our own directed graph
      doc.sentences(offset).setDependencies(DependencyMap.STANFORD_BASIC, CoreNLPUtils.toDirectedGraph(basicDeps, in))
      doc.sentences(offset).setDependencies(DependencyMap.STANFORD_COLLAPSED, CoreNLPUtils.toDirectedGraph(collapsedDeps, in))

      //println("Output directed graph:")
      //println(dg)

      offset += 1
    }
  }

  /** Parses one sentence and stores the dependency graph in the sentence object */
  private def parseSentence(sentence:Sentence):DirectedGraph[String] = {
    // tokens stores the tokens in the input format expected by malt (CoNLL-X)
    val tokens = new Array[String](sentence.words.length)
    for(i <- tokens.indices) {
      tokens(i) = s"${i + 1}\t${sentence.words(i)}\t${sentence.lemmas.get(i)}\t${sentence.tags.get(i)}\t${sentence.tags.get(i)}\t_"
    }

    // the actual parsing
    val output = getService.parseTokens(tokens)

    // convert malt's output into our dependency graph
    val edgeBuffer = new ListBuffer[(Int, Int, String)]
    val roots = new mutable.HashSet[Int]
    for(o <- output) {
      //println(o)
      val tokens = o.split("\\s+")
      if(tokens.length < 8)
        throw new RuntimeException("ERROR: Invalid malt output line: " + o)
      // malt indexes tokens from 1; we index from 0
      val modifier = tokens(0).toInt - 1
      val head = tokens(6).toInt - 1
      val label = tokens(7).toLowerCase

      // sometimes malt generates dependencies from root with a different label than "root"
      // not sure why this happens, but let's manage this: create a root node for all
      // if(label == "root" && head == -1) {
      if(head == -1) {
        roots += modifier
      } else {
        edgeBuffer += new Tuple3(head, modifier, in(label))
      }
    }

    new DirectedGraph[String](edgeBuffer.toList, roots.toSet)
  }

  private def getService:MaltParserService = {
    if(maltService.get() == null) {
      val service = new maltparserx.MaltParserService()
      service.initializeParserModel(mkArgs(
        Files.mkTmpDir("maltwdir", deleteOnExit = true),
        DEFAULT_MODEL_NAME))
      maltService.set(service)
    }
    maltService.get()
  }

  private def mkArgs(workDir:String, modelName:String):String = {
    val args = new ArrayBuffer[String]()

    args += "-m"
    args += "parse"

    args += "-w"
    args += workDir

    args += "-c"
    args += modelName

    args += "-v"
    args += "error"

    args.mkString(" ")
  }

  override def discourse(doc:Document) {
    if(! withDiscourse) return
    basicSanityCheck(doc, checkAnnotation = false)

    if (doc.sentences.head.tags.isEmpty)
      throw new RuntimeException("ERROR: you have to run the POS tagger before discourse parsing!")
    if (doc.sentences.head.lemmas.isEmpty)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before discourse parsing!")
    if(! hasDeps(doc.sentences.head))
      throw new RuntimeException("ERROR: you have to run the dependency parser before discourse parsing!")

    val out = rstDependencyParser.parse(doc)
    doc.discourseTree = Some(out._1)

    //println("FOUND DISCOURSE TREE:\n" + out._1)
  }
}

object FastNLPProcessor {
  val DEFAULT_MODEL_NAME = "nivreeager-en-crammer"

  var stanfordDependencyParser:Option[DependencyParser] = None

  def fetchStanfordParser():DependencyParser = {
    this.synchronized {
      if(stanfordDependencyParser.isEmpty)
        stanfordDependencyParser = Some(DependencyParser.loadFromModelFile(DependencyParser.DEFAULT_MODEL, new Properties()))
      stanfordDependencyParser.get
    }
  }

  var rstParser:Option[RSTParser] = None

  def fetchRSTParser(path:String):RSTParser = {
    this.synchronized {
      if(rstParser.isEmpty) rstParser = Some(RSTParser.loadFrom(path))
      rstParser.get
    }
  }
}
