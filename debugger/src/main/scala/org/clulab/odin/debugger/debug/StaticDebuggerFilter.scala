package org.clulab.odin.debugger.debug

import org.clulab.odin.impl.Extractor

class StaticDebuggerFilter(f: StaticDebuggerFilter.FilterType) {

  def apply(extractor: Extractor): Boolean = f(extractor)
}

object StaticDebuggerFilter {
  type FilterType = Extractor => Boolean

  val trueFilter: StaticDebuggerFilter = {
    val f = (extractor: Extractor) => {
      true
    }

    StaticDebuggerFilter(f)
  }

  def apply(f: FilterType): StaticDebuggerFilter = new StaticDebuggerFilter(f)

  def extractorFilter(outerExtractor: Extractor): StaticDebuggerFilter = {
    val f = (innerExtractor: Extractor) => {
      outerExtractor.eq(innerExtractor)
    }

    StaticDebuggerFilter(f)
  }
}
