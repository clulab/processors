package org.clulab.processors.corenlp

import java.util
import scala.collection.JavaConverters._
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct._
import edu.stanford.nlp.ling.{ CoreAnnotations, CoreLabel }
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.trees.SemanticHeadFinder
import edu.stanford.nlp.trees.TreeCoreAnnotations.{BinarizedTreeAnnotation, TreeAnnotation}
import edu.stanford.nlp.util.{ArrayCoreMap, CoreMap}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import edu.stanford.nlp.trees.{Tree => StanfordTree}

/**
 * Utilities for manipulating CoreNLP data structures
 * User: mihais, gus
 * Date: 11/2016, 2/25/15
 */
object CoreNLPUtils {

  private def setAnnotations(w: CoreLabel, label: String): Unit = {
    w.setWord(label)
    w.setLemma(label)
    w.setValue(label)
  }

  /** generates the symbol for the provided word <br>
    * used to match the preprocessing performed by corenlp <br>
    * default is simply the word itself
    * */
  def wordToSymbol(word: String): String = word match {
    // convert parens to Penn Treebank symbols because this is what the parser has seen in training
    case "(" => "-LRB-"
    case ")" => "-RRB-"
      // do nothing
    case w => w
  }

  /** correct annotations to match settings used when training corenlp models */
  def normalizeAnnotations(words: java.util.List[CoreLabel]): java.util.List[CoreLabel] = {
    val processedWords = new util.ArrayList[CoreLabel]()
    for(w <- words) {
      val nw = new CoreLabel(w)
      // retrieve the symbol for the provided word
      // default is simply the word itself
      val word = nw.word()
      val symbol = wordToSymbol(word)
      if (symbol != word) setAnnotations(nw, symbol)
      processedWords.add(nw)
    }
    processedWords
  }

  def toDirectedGraph(sg:SemanticGraph, interning: (String) => String):DirectedGraph[String] = {
    val edgeBuffer = new ListBuffer[Edge[String]]
    for (edge <- sg.edgeIterable()) {
      val head:Int = edge.getGovernor.get(classOf[CoreAnnotations.IndexAnnotation])
      val modifier:Int = edge.getDependent.get(classOf[CoreAnnotations.IndexAnnotation])
      var label = edge.getRelation.getShortName
      val spec = edge.getRelation.getSpecific
      if (spec != null) label = label + "_" + spec
      edgeBuffer.add(Edge(head - 1, modifier - 1, interning(label)))
    }

    val roots = new mutable.HashSet[Int]
    for (iw <- sg.getRoots) {
      roots.add(iw.get(classOf[CoreAnnotations.IndexAnnotation]) - 1)
    }

    val dg = new DirectedGraph[String](edgeBuffer.toList, roots.toSet)
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
  def sentenceToCoreMap(s: Sentence): CoreMap = {

    val coreLabels: Seq[CoreLabel] = for {
      (w: String, i: Int) <- s.words.zipWithIndex
    } yield {
      val crtTok = new CoreLabel()
      // set word
      crtTok.setWord(wordToSymbol(w))
      crtTok.setValue(w)
      // set tag
      if (s.tags.nonEmpty) crtTok.setTag(s.tags.get(i))
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
    sa.set(classOf[CoreAnnotations.TokensAnnotation], coreLabels.toList.asJava)

    sa
  }

  def sentenceToAnnotation(s: Sentence): Annotation = new Annotation(List(sentenceToCoreMap(s)))

  /**
   * Creates an Annotation (one per sentence) from a Document
   */
  def docToAnnotation(doc: Document): Annotation = {
    val cms = for {
      s <- doc.sentences
    } yield sentenceToCoreMap(s)
    new Annotation(cms.toList.asJava)
  }
}
