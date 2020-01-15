package org.clulab.ctxemb

import java.io.PrintWriter

import scala.collection.mutable

/**
 * Generates embeddings for lemmas, by averaging GloVe embeddings for words that have the same lemma
 * The averaging of embedding vectors is weighted by the frequency of the corresponding words in Gigaword
 *
 * @author Mihai
 */
class LemmatizeEmbeddings(val frequencyFile:String, val embeddingFile:String) {

  val frequencies = loadFreqFile()
  val wordEmbeddings = loadEmbeddings()

  def loadFreqFile(): Map[String, Double] = {
    val f = new mutable.HashMap[String, Double]()
    for(line <- io.Source.fromFile(frequencyFile).getLines()) {
      val toks = line.split("\\s+")
      assert(toks.length == 2)
      val word = toks(0)
      val freq = toks(1).toDouble / 10000.0 // to avoid overflows
      f += word -> freq
    }
    f.toMap
  }

  def loadEmbeddings(): Map[String, Array[Double]] = {
    val e = new mutable.HashMap[String, Array[Double]]()
    for(line <- io.Source.fromFile(embeddingFile).getLines()) {
      val toks = line.split("\\s+")
      assert(toks.length > 2)
      val word = toks(0)
      val vector = new Array[Double](toks.length - 1)
      for(i <- 1 until toks.length) {
        vector(i - 1) = toks(i).toDouble
      }
    }
    e.toMap
  }

  def lemmatize(): Map[String, Array[Double]] = {
    val ne = new mutable.HashMap[String, Array[Double]]()
    ne.toMap
  }
}

object LemmatizeEmbeddings {
  def main(args: Array[String]): Unit = {
    val freqFile = args(0)
    val embedFile = args(1)
    val outputFile = embedFile + "_lemmas"

    val le = new LemmatizeEmbeddings(freqFile, embedFile)
    val lemmaEmbeddings = le.lemmatize()

    val pw = new PrintWriter(outputFile)
    for(lemma <- lemmaEmbeddings.keySet.toList.sorted) {
      pw.print(lemma)
      val v = lemmaEmbeddings(lemma)
      for(i <- v.indices) {
        pw.print(" " + v(i))
      }
      pw.println()
    }
    pw.close()
  }
}
