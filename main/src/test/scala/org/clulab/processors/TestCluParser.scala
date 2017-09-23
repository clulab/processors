package org.clulab.processors

import java.io.{BufferedReader, InputStreamReader}

import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.syntax.EvaluateMalt
import org.scalatest.{FlatSpec, Matchers}

/**
  * Integration test for the Clu parser
  * User: mihais
  * Date: 7/26/17
  */
class TestCluParser extends FlatSpec with Matchers {
  val proc = new CluProcessor()

  "CluProcessor" should "parse WSJ with an accuracy over 89%" in {
    val model = proc.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/wsj_test.conllx")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"WSJ performance: $las, $uas")
    reader.close()
    (las > 0.89) should be (true)
    (uas > 0.90) should be (true)
  }

  it should "parse Genia with an accuracy over 87%" in {
    val model = proc.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/genia_test.conllx")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"Genia performance: $las, $uas")
    reader.close()
    (las > 0.87) should be (true)
    (uas > 0.88) should be (true)
  }
}
