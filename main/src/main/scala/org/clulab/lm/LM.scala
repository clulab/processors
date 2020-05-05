package org.clulab.lm

import java.io.PrintWriter

import edu.cmu.dynet.Expression

/** Trait for language model (LM) functionality */
trait LM {
  // produce an encoding of a text, with optional POS tags, and position embeddings relative to a predicate
  def mkEmbeddings(words: Iterable[String], posTags: Option[Iterable[String]], predPosition: Option[Int], doDropout:Boolean): (Iterable[Expression], Iterable[Expression])

  // the dimension of the word state
  def dimensions: Int

  // the dimension of the word embedding
  def wordDimensions: Int

  def saveX2i(printWriter: PrintWriter)
}


