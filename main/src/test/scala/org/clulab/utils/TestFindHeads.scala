package org.clulab.utils

import org.clulab.processors.Sentence
import org.clulab.serialization.json.JSONSerializer
import org.clulab.struct.{DirectedGraph, Edge, Interval}

import java.io.File

class TestFindHeads extends Test {

  def newSentence(words: Seq[String]): Sentence = ???

  behavior of "DirectedGraph"

  it should "not suffer from issue #134" in {
    println(new File(".").getAbsolutePath)
    val file = new File("./main/src/test/resources/PMC3747764.json")
    val document = JSONSerializer.toDocument(file)
    val sentence = document.sentences(679)
    // this sentence has a single root (token 1)
    val roots = sentence.dependencies.get.roots
    val tokenInterval = Interval(0, sentence.size)
    // SPOILER: this fails
    DependencyUtils.findHeadsStrict(tokenInterval, sentence)
  }
}
