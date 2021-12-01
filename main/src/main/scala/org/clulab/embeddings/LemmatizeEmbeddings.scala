package org.clulab.embeddings

import java.io.PrintWriter

import org.clulab.processors.clu.tokenizer.EnglishLemmatizer
import org.clulab.struct.Counter

import scala.collection.mutable

/**
 * Generates embeddings for lemmas, by averaging GloVe embeddings for words that have the same lemma
 * The averaging of embedding vectors is weighted by the frequency of the corresponding words in Gigaword
 *
 * @author Mihai
 */
class LemmatizeEmbeddings(val frequencyFile:String,
                          val embeddingFile:String) {

  val frequencies = loadFreqFile()
  val wordEmbeddings = loadEmbeddings()

  def loadFreqFile(): Map[String, Double] = {
    val f = new mutable.HashMap[String, Double]()
    for(line <- io.Source.fromFile(frequencyFile).getLines()) {
      val toks = line.split("\\s+")
      assert(toks.length == 2)
      val word = toks(0)
      val freq = toks(1).toDouble
      f += word -> freq
    }
    println(s"Loaded frequencies for ${f.keySet.size} words.")
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
      //println(s"Loaded embedding for ${word}")
      e += word -> vector
    }
    println(s"Loaded embeddings for ${e.keySet.size} words.")
    e.toMap
  }

  def lemmatize(): Map[String, Array[Double]] = {
    val lemmatizer = new EnglishLemmatizer
    val ne = new mutable.HashMap[String, Array[Double]]()
    val totalWeights = new Counter[String]()
    var totalUnk = 0
    for(word <- wordEmbeddings.keySet) {
      val lemma = lemmatizer.lemmatizeWord(word)
      val lowerCaseWord = word.toLowerCase()
      val vector = wordEmbeddings(word)

      // known word
      if(frequencies.contains(lowerCaseWord)) { // our counts are for lower case words...
        // println(s"[$word] lemmatized to [$lemma]")
        val weight = frequencies(lowerCaseWord)
        multiply(vector, weight) // in place, but this is Ok: we only see each vector once
        totalWeights.incrementCount(lemma, weight)
        add(ne, lemma, vector)
      }

      // unknown word in Gigaword => add vector to the UNK token
      else {
        totalWeights.incrementCount(SanitizedWordEmbeddingMap.UNK)
        add(ne, SanitizedWordEmbeddingMap.UNK, vector)
      }
    }

    // normalize
    for(lemma <- ne.keySet) {
      val totalWeight = totalWeights.getCount(lemma)
      val vector = ne(lemma)
      divide(vector, totalWeight)
    }

    println(s"Processed ${wordEmbeddings.keySet.size} words, and found $totalUnk unknown words.")

    ne.toMap
  }

  def multiply(v:Array[Double], s:Double): Unit = {
    for(i <- v.indices) {
      v(i) *= s
    }
  }

  def divide(v:Array[Double], s:Double): Unit = {
    for(i <- v.indices) {
      v(i) /= s
    }
  }

  def add(e: mutable.HashMap[String, Array[Double]], lemma:String, v:Array[Double]): Unit = {
    if(e.contains(lemma)) {
      val ev = e(lemma)
      assert(ev.length == v.length)
      for(i <- ev.indices) {
        ev(i) += v(i)
      }
    } else {
      val nv = new Array[Double](v.length)
      for(i <- v.indices) {
        nv(i) = v(i)
      }
      e += lemma -> nv
    }
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
    for(lemma <- lemmaEmbeddings.keySet) {
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
