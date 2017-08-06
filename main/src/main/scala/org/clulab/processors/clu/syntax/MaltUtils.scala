package org.clulab.processors.clu.syntax

import org.clulab.processors.{Processor, Sentence}
import org.clulab.struct.{DirectedGraph, Edge}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Utils necessary for malt parsing
  * User: mihais
  * Date: 8/6/17
  */
object MaltUtils {
  def sentenceToConllx(sentence:Sentence): Array[String] = {
    // tokens stores the tokens in the input format expected by malt (CoNLL-X)
    val inputTokens = new Array[String](sentence.words.length)

    //println(s"WORDS: ${sentence.words.mkString(", ")}")
    //println(s"LEMMAS: ${sentence.lemmas.get.mkString(", ")}")
    //println(s"TAGS: ${sentence.tags.get.mkString(", ")}")
    for(i <- inputTokens.indices) {
      inputTokens(i) = s"${i + 1}\t${sentence.words(i)}\t${sentence.lemmas.get(i)}\t${sentence.tags.get(i)}\t${sentence.tags.get(i)}\t_"
    }

    inputTokens
  }

  def conllxToDirectedGraph(tokens:Array[String], internStrings:Boolean): DirectedGraph[String] = {
    val edgeBuffer = new ListBuffer[Edge[String]]
    val roots = new mutable.HashSet[Int]
    for(o <- tokens) {
      //println(o)
      val tokens = o.split("\\s+")
      if(tokens.length < 8)
        throw new RuntimeException("ERROR: Invalid malt output line: " + o)
      // malt indexes tokens from 1; we index from 0
      val modifier = tokens(0).toInt - 1
      val head = tokens(6).toInt - 1
      val label = tokens(7).toLowerCase

      // sometimes malt generates dependencies from root with a different label than "root"
      // not sure why this happens, but let's manage this: create a root node in these cases
      if(head == -1) {
        roots += modifier
      } else {
        edgeBuffer += Edge(source = head, destination = modifier, relation = in(label, internStrings))
      }
    }

    new DirectedGraph[String](edgeBuffer.toList, roots.toSet)
  }

  def in(s:String, internStrings:Boolean):String = {
    if (internStrings) Processor.internString(s)
    else s
  }
}
