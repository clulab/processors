package org.clulab.processors.coreference

import org.clulab.openie.utils.TagSet
import org.clulab.utils.Lazy
import org.clulab.utils.Test

class TestExample extends Test {

  behavior of "subproject"

  it should "have access to main" in {
    // Grab something from the main project.
    val lazyInt = Lazy(5)
    lazyInt.value should be (5)
  }

  it should "have access to openie" in {
    // Grab something from the openie project.
    val tagSet = TagSet("english")
    tagSet.isAnyNoun("N") should be (true)
  }
}
