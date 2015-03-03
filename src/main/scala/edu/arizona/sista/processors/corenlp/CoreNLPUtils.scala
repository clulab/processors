package edu.arizona.sista.processors.corenlp

import java.util

import edu.arizona.sista.struct._
import edu.stanford.nlp.ling.CoreAnnotations.IndexAnnotation
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.trees.SemanticHeadFinder
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import edu.stanford.nlp.trees.{Tree => StanfordTree}

/**
 * Utilities for manipulating CoreNLP data structures
 * User: mihais
 * Date: 2/25/15
 */
object CoreNLPUtils {
  private def setWord(w:CoreLabel, label:String): Unit = {
    w.setWord(label)
    w.setLemma(label)
    w.setValue(label)
  }

  def parensToSymbols(words:java.util.List[CoreLabel]):java.util.List[CoreLabel] = {
    val processedWords = new util.ArrayList[CoreLabel]()
    for(w <- words) {
      val nw = new CoreLabel(w)
      if(nw.word() == "(") {
        setWord(nw, "-LRB-")
      }
      else if(nw.word() == ")") {
        setWord(nw, "-RRB-")
      }
      processedWords.add(nw)
    }
    processedWords
  }

  def toDirectedGraph(sg:SemanticGraph, interning: (String) => String):DirectedGraph[String] = {
    val edgeBuffer = new ListBuffer[(Int, Int, String)]
    for (edge <- sg.edgeIterable()) {
      val head:Int = edge.getGovernor.get(classOf[IndexAnnotation])
      val modifier:Int = edge.getDependent.get(classOf[IndexAnnotation])
      var label = edge.getRelation.getShortName
      val spec = edge.getRelation.getSpecific
      if (spec != null) label = label + "_" + spec
      edgeBuffer.add((head - 1, modifier - 1, interning(label)))
    }

    val roots = new mutable.HashSet[Int]
    for (iw <- sg.getRoots) {
      roots.add(iw.get(classOf[IndexAnnotation]) - 1)
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
}
