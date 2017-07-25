package org.clulab.odin

import org.scalatest.{ Matchers, FlatSpec }

class TestVariables extends FlatSpec with Matchers {

  def readResource(filename: String): String = {
    val source = io.Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }

  "variables" should "allow for whitespace" in {

    val mf =
      """
        |vars:
        |  rule_name_prefix: "test"
        |
        |rules:
        |  - name: "${ rule_name_prefix     }-1"
        |    label: Test
        |    type: token
        |    pattern: test
      """.stripMargin

    val ee = ExtractorEngine(mf)
    ee.extractors should have size (1)
  }

  it should "be applied to imports" in {

    val mf =
      """
        |vars:
        |  path_prefix: org/clulab/odin/variables
        |  my_prefix: "highest-precedence"
        |
        |rules:
        |  - import: ${ path_prefix }/test_imports.yml
      """.stripMargin

    val ee = ExtractorEngine(mf)
    ee.extractors should have size (1)
  }

  it should "resolve variables in order of precedence" in {

    val mf =
      """
        |vars:
        |  path_prefix: org/clulab/odin/variables
        |  my_prefix: "highest-precedence"
        |
        |rules:
        |  - import: ${ path_prefix }/test_imports.yml
        |    vars:
        |      name_prefix: ${my_prefix } # should be able to resolve from vars block
      """.stripMargin

    val ee = ExtractorEngine(mf)
    ee.extractors should have size (1)
    println(ee.extractors.head.name)
    ee.extractors.head.name should startWith ("highest-precedence")
  }

  it should "support nested resolution" in {

    val mf =
      """
        |vars:
        |  n1: n2
        |  n2: "success"
        |
        |rules:
        |  - name: "${ ${ n1}     }-1"  # name should equal to "success-1" after nested substitutions
        |    label: Test
        |    type: token
        |    pattern: test
      """.stripMargin

    val ee = ExtractorEngine(mf)
    ee.extractors should have size (1)
    println(ee.extractors.head.name)
    ee.extractors.head.name should startWith ("success")
  }
}
