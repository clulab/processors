package org.clulab.processors.corenlp.parser

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.Edge


object TrainParser extends App {

  // print some dependencies
  dependencyExample


  /**
    * An example demonstrating how to access the dependencies in a [[org.clulab.processors.Sentence]]
    */
  def dependencyExample: Unit = {

    // initialize a processor for tagging, parsing, etc.
    val proc = new FastNLPProcessor(withChunks = false)

    val text = "I saw the cat with the telescope."
    // tokenize, tag, parse, etc.
    val doc = proc.annotate(text)

    // sentences is an array of sentences.
    // For the example text, its length should be 1
    println(s"doc.sentences.size: ${doc.sentences.size}")

    // let's examine the first (and only) sentence
    val s = doc.sentences.head

    // get the words for this sentence
    val words = s.words
    // retrieve the collapsed and basic dependencies.
    // I imagine your training data will be in the form of "basic" Stanford dependencies
    val collapsedDeps = s.stanfordCollapsedDependencies.get
    val basicDeps = s.stanfordBasicDependencies.get

    // We'll define a function to format an edge of the dependency graph
    def formatEdge(e: Edge[String], words: Array[String]): String = {
      val source = words(e.source)
      val destination = words(e.destination)
      s"""${e.relation}($source, $destination)"""
    }

    // display the dependencies
    println(text)
    println(s"""Basic dependencies:\n${collapsedDeps.edges.map(e => s"\t${formatEdge(e, words)}").mkString("\n")}""")
    println(s"""Collapsed dependencies:\n${collapsedDeps.edges.map(e => s"\t${formatEdge(e, words)}").mkString("\n")}""")
  }
}
