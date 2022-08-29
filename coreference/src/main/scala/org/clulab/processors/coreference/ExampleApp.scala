package org.clulab.processors.coreference

import org.clulab.openie.utils.TagSet
import org.clulab.utils.Lazy

object ExampleApp extends App {
  // Grab something from the main project.
  val lazyInt = Lazy(5)
  println(lazyInt.value)

  // Grab something from the openie project.
  val tagSet = TagSet("english")
  println(tagSet.isAnyNoun("N"))
}
