package org.clulab.odin.debugger.debug.filter

import org.clulab.odin.impl.Extractor

class StaticDebuggerFilter(f: StaticDebuggerFilter.FilterType) extends Filter[StaticDebuggerFilter.ArgumentType] {

  def apply(extractor: Extractor): Boolean = f(extractor)

  def and(other: StaticDebuggerFilter): StaticDebuggerFilter =
      StaticDebuggerFilter.and(this, other)

  def or(other: StaticDebuggerFilter): StaticDebuggerFilter =
      StaticDebuggerFilter.or(this, other)

  def not: StaticDebuggerFilter = StaticDebuggerFilter.not(this)
}

object StaticDebuggerFilter {
  type ArgumentType = Extractor
  type FilterType = ArgumentType => Boolean

  val trueFilter: StaticDebuggerFilter = {
    val f = (extractor: Extractor) => {
      true
    }

    StaticDebuggerFilter(f)
  }

  def apply(f: FilterType): StaticDebuggerFilter = new StaticDebuggerFilter(f)

  def and(left: StaticDebuggerFilter, right: StaticDebuggerFilter): StaticDebuggerFilter = {
    val f = (extractor: ArgumentType) => {
      left(extractor) && right(extractor)
    }

    StaticDebuggerFilter(f)
  }

  def or(left: StaticDebuggerFilter, right: StaticDebuggerFilter): StaticDebuggerFilter = {
    val f = (extractor: ArgumentType) => {
      left(extractor) || right(extractor)
    }

    StaticDebuggerFilter(f)
  }

  def not(filter: StaticDebuggerFilter): StaticDebuggerFilter = {
    val f = (extractor: ArgumentType) => {
      !filter(extractor)
    }

    StaticDebuggerFilter(f)
  }

  def extractorFilter(outerExtractor: Extractor): StaticDebuggerFilter = {
    val f = (innerExtractor: Extractor) => {
      outerExtractor.eq(innerExtractor)
    }

    StaticDebuggerFilter(f)
  }
}
