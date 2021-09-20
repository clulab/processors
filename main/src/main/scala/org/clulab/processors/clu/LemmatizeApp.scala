package org.clulab.processors.clu

import org.clulab.dynet.Utils

object LemmatizeApp extends App {
  Utils.initializeDyNet()

  val processor = new CluProcessor()
  val doc = processor.mkDocument("counties")

  processor.mkConstEmbeddings(doc)
  processor.lemmatize(doc)
}
