package org.clulab.processors.fastnlp

import org.clulab.processors.Document
import org.clulab.struct.GraphMap
import org.clulab.discourse.rstparser.RSTParser
import org.clulab.discourse.rstparser.Utils._
import org.clulab.processors.corenlp.CoreNLPUtils
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.parser.nndep.DependencyParser
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.semgraph.SemanticGraphFactory
import edu.stanford.nlp.trees.GrammaticalStructure
import java.util.Properties
import scala.collection.JavaConversions._
import FastNLPProcessor._


/**
 * Fast NLP tools
 * Extends ShallowNLP with a dependency parser based on the Stanford NN dependency parser
 * This means that constituent trees and coreference (which depends on constituent syntax) are not available
 * The default setting is to use the Stanford parser with "basic" dependencies
 * User: mihais
 * Date: 1/4/14
 */
class FastNLPProcessor(
  internStrings:Boolean = true,
  withChunks:Boolean = true,
  withDiscourse:Int = ShallowNLPProcessor.NO_DISCOURSE) extends ShallowNLPProcessor(internStrings) {

  /** RST discourse parser using only dependency based syntax */
  lazy val rstDependencyParser = fetchRSTParser(RSTParser.DEFAULT_DEPENDENCYSYNTAX_MODEL_PATH)

  /** Stanford's NN dependency parser */
  lazy val stanfordDepParser = fetchStanfordParser()

  override def parse(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return
    if (doc.sentences.head.tags.isEmpty)
      throw new RuntimeException("ERROR: you have to run the POS tagger before parsing!")
    if (doc.sentences.head.lemmas.isEmpty)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before parsing!")

    parseWithStanford(doc, annotation.get)
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
      doc.sentences(offset).setDependencies(GraphMap.STANFORD_BASIC, CoreNLPUtils.toDirectedGraph(basicDeps, in))
      doc.sentences(offset).setDependencies(GraphMap.STANFORD_COLLAPSED, CoreNLPUtils.toDirectedGraph(collapsedDeps, in))

      //println("Output directed graph:")
      //println(dg)

      offset += 1
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

    val out = rstDependencyParser.parse(doc, withDiscourse == ShallowNLPProcessor.JUST_EDUS)
    doc.discourseTree = Some(out._1)

    //println("FOUND DISCOURSE TREE:\n" + out._1)
  }
}

object FastNLPProcessor {
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
