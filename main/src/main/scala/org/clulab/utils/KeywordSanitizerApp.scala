package org.clulab.utils

import org.clulab.processors.clu.CluProcessor

object KeywordSanitizerApp extends App {
  val proc = new CluProcessor()
  val text = "Joe Biden is the president of the United States."
  val sent = proc.annotate(text).sentences(0)
  val sanitizer = new KeywordSanitizer()
  val sanitized = sanitizer.sanitize(sent)
  println(sanitized.mkString(", "))
}
