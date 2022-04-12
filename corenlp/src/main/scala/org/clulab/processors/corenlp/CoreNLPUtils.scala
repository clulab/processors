package org.clulab.processors.corenlp

import java.util
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct._
import edu.stanford.nlp.ling.CoreAnnotations.{IndexAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.trees.SemanticHeadFinder
import edu.stanford.nlp.trees.TreeCoreAnnotations.{BinarizedTreeAnnotation, TreeAnnotation}
import edu.stanford.nlp.util.{ArrayCoreMap, CoreMap}

import edu.stanford.nlp.trees.{Tree => StanfordTree}
import org.clulab.processors.clu.tokenizer.TokenizerStepNormalization

import scala.collection.JavaConverters._

/**
 * Utilities for manipulating CoreNLP data structures
 * User: mihais
 * Date: 2/25/15
 * Last Modified: Update for Scala 2.12: java converters.
 */
object CoreNLPUtils {

  private def setWord(w:CoreLabel, label:String): Unit = {
    w.setWord(label)
    w.setLemma(label)
    w.setValue(label)
  }

  def parensToSymbols(words:java.util.List[CoreLabel]):java.util.List[CoreLabel] = {
    val processedWords = new util.ArrayList[CoreLabel]()
    for(w <- words.asScala) {
      val nw = new CoreLabel(w)
      if(TokenizerStepNormalization.PARENS.contains(nw.word())) {
        setWord(nw, TokenizerStepNormalization.PARENS(nw.word()))
      }
      processedWords.add(nw)
    }
    processedWords
  }

  def toDirectedGraph(sg: SemanticGraph, interning: (String) => String, preferredSize: Option[Int] = None, debug: Boolean = false): DirectedGraph[String] = {

    def mkEdges(): List[Edge[String]] = {
      // Using list.distinct instead of Set.toList will preserve the order of edges.
      // Set can be sorted in different ways even on different runs with the same data.
      sg.edgeIterable().asScala.toList.map { edge =>
        val head: Int = edge.getGovernor.get(classOf[IndexAnnotation])
        val modifier: Int = edge.getDependent.get(classOf[IndexAnnotation])
        val specOpt = Option(edge.getRelation.getSpecific)
        val label = edge.getRelation.getShortName + specOpt.map("_" + _).getOrElse("")
        if (debug)
          println(s"Adding the following dependency: (${head - 1}, ${modifier - 1}, $label)")
        // Of the Edge, the source is head - 1, the destination is modifier - 1.
        Edge(head - 1, modifier - 1, interning(label))
      }.distinct
      // An extra distinct needs to be called on edges because CoreNLP sometimes duplicates the enhanced deps it creates.
    }

    val edges = mkEdges()
    val dg = new DirectedGraph[String](edges, preferredSize)
    //println(dg)
    dg
  }

  def toTree(stanfordTree:StanfordTree,
             headFinder: SemanticHeadFinder,
             position:MutableNumber[Int]):Tree = {
    assert(stanfordTree != null)

    if (stanfordTree.isLeaf) {
      val tree = Terminal(stanfordTree.label.value())
      tree.setIndex(position.value)
      position.value += 1
      return tree
    }

    // println("Converting tree: " + stanfordTree.toString)
    val children = new Array[Tree](stanfordTree.numChildren())
    for (i <- 0 until stanfordTree.numChildren()) {
      children(i) = toTree(stanfordTree.getChild(i), headFinder, position)
    }
    val value = stanfordTree.label.value()
    val start = children(0).startOffset
    val end = children(children.length - 1).endOffset

    val headDaughter = headFinder.determineHead(stanfordTree)
    var head = -1
    var i = 0
    while(i < stanfordTree.numChildren() && head == -1) {
      if (headDaughter == stanfordTree.getChild(i)) {
        head = i
      }
      i += 1
    }

    val nt = NonTerminal(value, children)
    nt.setStartEndIndices(start, end)
    nt.setHead(head)
    nt
  }

  /**
   * Create an Annotation from a Sentence
   */
  def sentenceToAnnotation(s: Sentence): Annotation = {

    val coreLabels: Seq[CoreLabel] = for {
      (w: String, i: Int) <- s.words.zipWithIndex
    } yield {
      val crtTok = new CoreLabel()
      // set word
      crtTok.setWord(w)
      crtTok.setValue(w)
      // set lemma
      if (s.lemmas.nonEmpty) crtTok.setLemma(s.lemmas.get(i))
      // set ner
      if (s.entities.nonEmpty) crtTok.setNER(s.entities.get(i))
      // set positions
      crtTok.setBeginPosition(s.startOffsets(i))
      crtTok.setEndPosition(s.endOffsets(i))
      crtTok.setIndex(i + 1) // Stanford counts tokens starting from 1
      crtTok.setSentIndex(i) // Stanford counts sentences starting from 0...
      crtTok
    }

    // attach the CoreLabels
    val sa: CoreMap = new ArrayCoreMap()
    sa.set(classOf[TokensAnnotation], coreLabels.toList.asJava)

    // TODO attach parse to sa

    // make Annotation
    val sas: util.List[CoreMap] = List(sa).asJava
    val annotation = new Annotation(sas)
    
    annotation
  }

  /**
   * Create an Annotation from a Document
   */
  def docToAnnotations(doc: Document): Seq[Annotation] = for {
    s <- doc.sentences
  } yield sentenceToAnnotation(s)
}
