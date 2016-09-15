package org.clulab.processors.corenlp

import java.util

import org.clulab.discourse.rstparser.RSTParser
import org.clulab.processors._
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.struct._
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.parser.common.{ParserAnnotations, ParserUtils}
import edu.stanford.nlp.pipeline.{ParserAnnotatorUtils, StanfordCoreNLP}
import java.util.Properties
import collection.mutable.ListBuffer
import edu.stanford.nlp.ling.CoreAnnotations._
import scala.collection.JavaConversions._
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
import edu.stanford.nlp.trees.{GrammaticalStructure, GrammaticalStructureFactory, SemanticHeadFinder}
import edu.stanford.nlp.trees.{Tree => StanfordTree}
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations
import org.clulab.discourse.rstparser.Utils._
import CoreNLPUtils._


/**
 * API for Stanford's CoreNLP tools
 * User: mihais
 * Date: 3/1/13
 */
class CoreNLPProcessor(
  internStrings:Boolean = true,
  withChunks:Boolean = true,
  val withDiscourse:Int = ShallowNLPProcessor.NO_DISCOURSE,
  val maxSentenceLength:Int = 100
) extends ShallowNLPProcessor(internStrings, withChunks) {

  lazy val coref = mkCoref
  lazy val rstConstituentParser = CoreNLPProcessor.fetchParser(RSTParser.DEFAULT_CONSTITUENTSYNTAX_MODEL_PATH)

  //
  // we maintain our own copy of a LexicalizedParser to control which sentences are parsed
  // the CoreNLP option parser.maxlen does not work well
  //
  lazy val stanfordParser = mkLexicalizedParser
  lazy val gsf = mkGSF
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

    val sas = annotation.get.get(classOf[SentencesAnnotation])
    var offset = 0
    for (sa <- sas) {
      // run the actual parser here
      val stanfordTree = stanfordParse(sa)

      // store Stanford annotations; Stanford dependencies are created here!
      ParserAnnotatorUtils.fillInParseAnnotations(false, true, gsf, sa, stanfordTree, GrammaticalStructure.Extras.NONE)

      // save our own structures
      if (stanfordTree != null) {
        // save the constituent tree, including head word information
        val position = new MutableNumber[Int](0)
        doc.sentences(offset).syntacticTree = Some(CoreNLPUtils.toTree(stanfordTree, headFinder, position))

        // save syntactic dependencies
        val basicDeps = sa.get(classOf[SemanticGraphCoreAnnotations.BasicDependenciesAnnotation])
        val collapsedDeps = sa.get(classOf[SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation])
        doc.sentences(offset).setDependencies(GraphMap.STANFORD_BASIC, CoreNLPUtils.toDirectedGraph(basicDeps, in))
        doc.sentences(offset).setDependencies(GraphMap.STANFORD_COLLAPSED, CoreNLPUtils.toDirectedGraph(collapsedDeps, in))
      } else {
        doc.sentences(offset).syntacticTree = None
      }
      offset += 1
    }
  }

  def stanfordParse(sentence:CoreMap):StanfordTree = {
    val constraints = sentence.get(classOf[ParserAnnotations.ConstraintAnnotation])
    val words = parensToSymbols(sentence.get(classOf[CoreAnnotations.TokensAnnotation]))

    var tree:StanfordTree = null

    //
    // Do not parse sentences longer than this
    // Those are likely coming from tables, so: (a) we don't know how to parse them anyway; (b) they would take forever
    //
    if(words.size < maxSentenceLength) {
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
            "Will ignore and continue: " + edu.stanford.nlp.ling.Sentence.listToString(words))
      }

      //println("SYNTACTIC TREE: " + tree)
    } else {
      System.err.println("Skipping sentence of length " + words.size)
    }

    // create a fake tree if the actual parsing failed
    if(tree == null)
      tree = ParserUtils.xTree(words)

    //println("TREE: " + tree)
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

      for (cid <- chains.keySet()) {
        // println("cluster " + cid)
        val mentionMap = chains.get(cid).getMentionMap
        for (mid <- mentionMap.keySet()) {
          for (mention <- mentionMap.get(mid)) {
            // val isRep = mention == cluster.getRepresentativeMention
            // println("\tmention " + mid.getSource + " " + mid.getTarget + " " + mention.startIndex + " " + mention.endIndex + " " + isRep + " [" + mention.mentionSpan + "]")

            // Processor indexes things from 0 not 1!
            val m = new CorefMention(
              mid.getSource - 1,
              mid.getTarget - 1,
              mention.startIndex - 1,
              mention.endIndex - 1,
              cid)
            mentions += m
          }
        }
      }

      doc.coreferenceChains = Some(new CorefChains(mentions.toList))
    }
  }

  override def discourse(doc:Document) {
    if(withDiscourse == ShallowNLPProcessor.NO_DISCOURSE) return
    basicSanityCheck(doc, checkAnnotation = false)

    if (doc.sentences.head.tags.isEmpty)
      throw new RuntimeException("ERROR: you have to run the POS tagger before discourse parsing!")
    if (doc.sentences.head.lemmas.isEmpty)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before discourse parsing!")
    if(! hasDeps(doc.sentences.head))
      throw new RuntimeException("ERROR: you have to run the dependency parser before discourse parsing!")
    if(doc.sentences.head.syntacticTree.isEmpty)
      throw new RuntimeException("ERROR: you have to run the constituent parser before discourse parsing!")

    val out = rstConstituentParser.parse(doc, withDiscourse == ShallowNLPProcessor.JUST_EDUS)
    doc.discourseTree = Some(out._1)

    //println("FOUND DISCOURSE TREE:\n" + out._1)
  }
}

object CoreNLPProcessor {
  var rstParser:Option[RSTParser] = None

  def fetchParser(path:String):RSTParser = {
    this.synchronized {
      if(rstParser.isEmpty) rstParser = Some(RSTParser.loadFrom(path))
      rstParser.get
    }
  }
}
