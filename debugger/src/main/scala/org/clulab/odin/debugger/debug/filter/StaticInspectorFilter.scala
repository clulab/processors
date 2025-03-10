package org.clulab.odin.debugger.debug.filter

import org.clulab.odin.impl.Extractor

class StaticInspectorFilter extends InspectorFilter {
  def showRuleView(extractor: Extractor): Boolean = true
  def showTextualView(extractor: Extractor): Boolean = true
  def showGraphicalView(extractor: Extractor): Boolean = true
}

object StaticInspectorFilter {
  val verbose = new StaticInspectorFilter()
  val concise = new StaticInspectorFilter {
    override def showGraphicalView(extractor: Extractor): Boolean = false
  }
}
