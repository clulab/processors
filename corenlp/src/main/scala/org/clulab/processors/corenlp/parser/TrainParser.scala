package org.clulab.processors.corenlp.parser

import java.io.{BufferedReader, FileInputStream, InputStreamReader}

import com.typesafe.config.ConfigFactory
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.{Edge, GraphMap}


object TrainParser extends App {

  val config = ConfigFactory.load()
  val file = config.getString("corenlp.parser.genia.testFile")
  val r = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"))

  println(s"Reading ${file}...\n")
  val reader = new ConllxReader
  val doc = reader.load(r)
  r.close()

  val copy = CoreNLPDocument(doc.sentences, doc.annotation.get)

  val proc = new FastNLPProcessor(withChunks = false)
  proc.tagPartsOfSpeech(copy)
  proc.lemmatize(copy)
  proc.parse(copy)

  var results = EvaluateUtils.Performance(0,0,0,0,"WithEdgeLabel")
  for (i <- doc.sentences.indices) {
    results += EvaluateUtils.evaluate(
      doc.sentences(i).dependenciesByType(GraphMap.STANFORD_BASIC),
      copy.sentences(i).dependenciesByType(GraphMap.STANFORD_BASIC),
      withEdgeLabel = true
    )
  }

  println(s"Results: tp: ${results.tp}, fp: ${results.fp}, tn: ${results.tn}, fn: ${results.fn}")
  println(s"Precision: ${results.precision}")
  println(s"Recall: ${results.recall}")
  println(s"F1: ${results.f1}")


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
    println(s"""Basic dependencies:\n${basicDeps.edges.map(e => s"\t${formatEdge(e, words)}").mkString("\n")}""")
    println(s"""Collapsed dependencies:\n${collapsedDeps.edges.map(e => s"\t${formatEdge(e, words)}").mkString("\n")}""")
  }
}
