package org.clulab.processors.corenlp

import java.util

import org.clulab.processors._
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.struct._
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.parser.common.{ParserAnnotations, ParserConstraint, ParserUtils}
import edu.stanford.nlp.pipeline.{ParserAnnotatorUtils, StanfordCoreNLP}
import java.util.Properties

import collection.mutable.ListBuffer
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel, IndexedWord}
import edu.stanford.nlp.trees.{GrammaticalStructure, GrammaticalStructureFactory, SemanticHeadFinder, TreeCoreAnnotations, Tree => StanfordTree}
import edu.stanford.nlp.semgraph.{SemanticGraph, SemanticGraphCoreAnnotations}
import CoreNLPUtils._
import edu.stanford.nlp.coref.CorefCoreAnnotations.CorefChainAnnotation
import org.slf4j.{Logger, LoggerFactory}
import CoreNLPProcessor.logger
import org.clulab.processors.clu.tokenizer.TokenizerStep

import scala.collection.JavaConverters._

/**
 * API for Stanford's CoreNLP tools
 * User: mihais
 * Date: 3/1/13
 * Last Modified: Update for Scala 2.12: java converters.
 */
class CoreNLPProcessor(
  tokenizerPostProcessor:Option[TokenizerStep],
  internStrings:Boolean,
  withChunks:Boolean,
  withRelationExtraction:Boolean,
  val withDiscourse:Int,
  val maxSentenceLength:Int) extends ShallowNLPProcessor(tokenizerPostProcessor, internStrings, withChunks, withRelationExtraction) with OpenIEAnnotator {

  def this(internStrings:Boolean = true,
           withChunks:Boolean = true,
           withRelationExtraction:Boolean = false,
           withDiscourse:Int = ShallowNLPProcessor.NO_DISCOURSE,
           maxSentenceLength:Int = 100) {
    this(None, internStrings, withChunks, withRelationExtraction, withDiscourse, maxSentenceLength)
  }

  lazy val coref: StanfordCoreNLP = mkCoref

  //
  // we maintain our own copy of a LexicalizedParser to control which sentences are parsed
  // the CoreNLP option parser.maxlen does not work well
  //
  lazy val stanfordParser: LexicalizedParser = mkLexicalizedParser
  lazy val gsf:GrammaticalStructureFactory = mkGSF
  lazy val headFinder = new SemanticHeadFinder()

  def mkLexicalizedParser: LexicalizedParser = {
    val parser = LexicalizedParser.loadModel()
    parser
  }

  def mkGSF:GrammaticalStructureFactory = {
    val tlp = stanfordParser.getTLPParams.treebankLanguagePack
    val g = tlp.grammaticalStructureFactory(tlp.punctuationWordRejectFilter(), tlp.typedDependencyHeadFinder())
    g
  }

  def mkCoref: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "dcoref")
    new StanfordCoreNLP(props, false)
  }

  override def parse(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return

    val sas = annotation.get.get(classOf[SentencesAnnotation]).asScala
    var offset = 0
    for (sa <- sas) {
      // run the actual parser here
      val stanfordTree = stanfordParse(sa)
      assert(stanfordTree != null) // even when we can't parse, we create a fake tree in stanfordParse()

      // store Stanford annotations; Stanford dependencies are created here!
      val words = sa.get(classOf[CoreAnnotations.TokensAnnotation])
      var depFail = false
      if(words.size() > 1) {
        //println("SENTENCE WORDS: ")
        //println(mkSentenceString(words))

        // note: this method fails on weird sentences, e.g., sentences that contain only: ")", "."
        // make sure we don't crash here!
        try {
          ParserAnnotatorUtils.fillInParseAnnotations(false, true, gsf, sa,
            util.Arrays.asList(stanfordTree), GrammaticalStructure.Extras.NONE)
        } catch {
          case e:Throwable =>
            logger.debug("WARNING: CoreNLP failed to extract syntactic dependencies on sentence: " + mkSentenceString(words))
            depFail = true
        }
      } else { // exactly 1 token in the sentence
        // save the constituent tree
        sa.set(classOf[TreeCoreAnnotations.TreeAnnotation], stanfordTree)

        // create and save a fake dependency graph
        val head = words.asScala.head
        val fakeDeps = new SemanticGraph()
        fakeDeps.addRoot(new IndexedWord(head))
        sa.set(classOf[SemanticGraphCoreAnnotations.BasicDependenciesAnnotation], fakeDeps)
        sa.set(classOf[SemanticGraphCoreAnnotations.EnhancedPlusPlusDependenciesAnnotation], fakeDeps)
      }

      // save the constituent tree in our doc, including head word information
      val position = new MutableNumber[Int](0)
      doc.sentences(offset).syntacticTree = Some(CoreNLPUtils.toTree(stanfordTree, headFinder, position))

      // save syntactic dependencies in our doc
      if(! depFail) {
        val basicDeps = sa.get(classOf[SemanticGraphCoreAnnotations.BasicDependenciesAnnotation])
        val enhancedDeps = sa.get(classOf[SemanticGraphCoreAnnotations.EnhancedPlusPlusDependenciesAnnotation])

        val sentenceSize = doc.sentences(offset).size
        doc.sentences(offset).setDependencies(GraphMap.UNIVERSAL_BASIC, CoreNLPUtils.toDirectedGraph(basicDeps, in, Some(sentenceSize)))
        doc.sentences(offset).setDependencies(GraphMap.UNIVERSAL_ENHANCED, CoreNLPUtils.toDirectedGraph(enhancedDeps, in, Some(sentenceSize)))
      }

      offset += 1
    }
  }

  def mkSentenceString(words:java.util.List[CoreLabel]): String = {
    val b = new StringBuilder
    for(i <- 0 until words.size()) {
      if(i > 0) b.append(" ")
      b.append(s"[${words.get(i).word()}]")
    }
    b.toString()
  }

  def stanfordParse(sentence:CoreMap):StanfordTree = {
    val constraints:java.util.List[ParserConstraint] = sentence.get(classOf[ParserAnnotations.ConstraintAnnotation])
    val words:java.util.List[CoreLabel] = parensToSymbols(sentence.get(classOf[CoreAnnotations.TokensAnnotation]))

    /*
    println("INPUT TO CONSTITUENT PARSER:")
    for(i <- 0 until words.size()) {
      println(i + ": " + words.get(i).word())
    }
    */

    var tree:StanfordTree = null

    //
    // Do not parse sentences that are too long or too short
    // The long ones are likely coming from tables, so: (a) we don't know how to parse them anyway; (b) they would take forever
    // Sentences of length 1 break the Stanford parser in version 3.8.0
    //
    if(words.size > 1 && words.size < maxSentenceLength) {
      // the actual parsing
      val pq = stanfordParser.parserQuery()
      pq.setConstraints(constraints)

      //print("Parsing sentence:")
      //for(w <- words) print(s" ${w.toString()}")
      //println()

      pq.parse(words)

      // fetch the best tree
      try {
        tree = pq.getBestParse
        if (tree != null)
          tree.setScore(pq.getPCFGScore % -10000.0)
      } catch {
        case e: Exception =>
          System.err.println("WARNING: Parsing of sentence failed, possibly because of out of memory. " +
            "Will ignore and continue: " + edu.stanford.nlp.ling.SentenceUtils.listToString(words, true))
      }

      //println("SYNTACTIC TREE: " + tree)
    } else {
      logger.debug("Skipping sentence of length " + words.size)
      tree = ParserUtils.xTree(words)
    }

    tree
  }

  override def resolveCoreference(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return

    if (doc.sentences.head.tags.isEmpty)
      throw new RuntimeException("ERROR: you have to run the POS tagger before coreference resolution!")
    if (doc.sentences.head.lemmas.isEmpty)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before coreference resolution!")
    if (doc.sentences.head.entities.isEmpty)
      throw new RuntimeException("ERROR: you have to run the NER before coreference resolution!")
    if(doc.sentences.head.syntacticTree.isEmpty)
      throw new RuntimeException("ERROR: you have to run the parser before coreference resolution!")

    coref.annotate(annotation.get)

    val chains = annotation.get.get(classOf[CorefChainAnnotation])
    if(chains != null) {
      val mentions = new ListBuffer[CorefMention]

      for (cid <- chains.keySet.asScala) {
       for(mention <- chains.get(cid).getMentionsInTextualOrder.asScala) {
          //println(s"""start = ${mention.startIndex}, end = ${mention.endIndex}, head = ${mention.headIndex}, sentence = ${mention.sentNum}""")
          val m = CorefMention(
           mention.sentNum - 1,
           mention.headIndex - 1,
           mention.startIndex - 1,
           mention.endIndex - 1,
           mention.corefClusterID
          )
          mentions += m
        }
      }

      //println(s"Mentions contains ${mentions.size} mentions.")
      doc.coreferenceChains = Some(new CorefChains(mentions.toList))
    }
  }

  override def discourse(doc:Document): Unit = {
    // no longer used
  }

  /** Semantic role labeling */
  override def srl(doc: Document): Unit = {
    // not implemented yet
  }
}

object CoreNLPProcessor {
  val logger: Logger = LoggerFactory.getLogger(classOf[CoreNLPProcessor])
}
