package org.clulab.lm

import edu.cmu.dynet.Expression

/** Trait for language model (LM) functionality */
trait LM {
  def mkEmbeddings(words: Iterable[String]): Iterable[Expression]
}


