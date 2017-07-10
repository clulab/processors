package org.clulab.odin

import org.scalatest.{ Matchers, FlatSpec }

class TestVariables extends FlatSpec with Matchers {

  def readResource(filename: String): String = {
    val source = io.Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }

  "variables" should "be replaced correctly" in {
    val rules = readResource("/org/clulab/odin/variables/master.yml")
    val ee = ExtractorEngine(rules)
    ee.extractors should have size (3)
    ee.extractors(0).name should be ("template_global_import_1")
    ee.extractors(1).name should be ("template_global_2")
    ee.extractors(2).name should be ("template_3")
  }

}
