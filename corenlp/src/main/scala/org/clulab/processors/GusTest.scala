package org.clulab.processors

import java.io.File

import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.processors.corenlp.parser.ParserUtils
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.utils.ConllxReader

// TODO: convert this to a unit test!

/**
  *
  * User: mihais
  * Date: 11/23/16
  */
object GusTest {
  def main(args:Array[String]): Unit = {
    val conllFile = new File("test.conllx.small")
    val doc = ConllxReader.load(conllFile)
    println(doc.sentences(0).words.zip(doc.sentences(0).tags.get).mkString(", "))

    val sentencesWithoutParses = doc.sentences.map(ParserUtils.copyWithoutDependencies)
    val coreDoc = CoreNLPDocument.fromSentences(sentencesWithoutParses)
    println(coreDoc.sentences(0).words.zip(coreDoc.sentences(0).tags.get).mkString(", "))

    val proc = new FastNLPProcessor()
    proc.tagPartsOfSpeech(coreDoc)
    proc.parse(coreDoc)
  }
}
