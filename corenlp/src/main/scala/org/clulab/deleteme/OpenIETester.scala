package org.clulab.deleteme

import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor

object OpenIETester extends App {
  val text = "Obama was born in Hawaii. He is our president."
  val processor = new CoreNLPProcessor

  val doc = processor.annotate(text)

  val x = 1

}
