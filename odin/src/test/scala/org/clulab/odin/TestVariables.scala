package org.clulab.odin

import scala.io.Source

import org.scalatest.{ Matchers, FlatSpec }


class TestVariables extends FlatSpec with Matchers {

  def readResource(filename: String): String = {
    val source = Source.fromURL(getClass.getResource(filename))
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
    ee.extractors.head.name should startWith ("test")
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
        |      name_prefix: ${my_prefix } # should be able to resolve from top-level vars block
      """.stripMargin

    val ee = ExtractorEngine.fromRules(mf)
    ee.extractors should have size (1)
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
        |  - name: "${ ${ n1}     }-1"  # value of name should be "success-1" after nested substitutions
        |    label: Test
        |    type: token
        |    pattern: test
      """.stripMargin

    val ee = ExtractorEngine.fromRules(mf)
    ee.extractors should have size (1)
    println(ee.extractors.head.name)
    ee.extractors.head.name should startWith ("success")
  }

  it should "disallow recursive definitions" in {

    val mf =
      """
        |vars:
        |  v1: ${v1}
        |
        |rules:
        |  - name: "${v1}"
        |    label: Test
        |    type: token
        |    pattern: test
      """.stripMargin

    an [java.lang.IllegalStateException] should be thrownBy ExtractorEngine.fromRules(mf)

  }

}
