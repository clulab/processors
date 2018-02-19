package org.clulab.processors

import java.io.{BufferedReader, InputStreamReader}

import org.clulab.processors.clu.{CluProcessor, CluProcessorWithStanford}
import org.clulab.processors.clu.syntax.EvaluateMalt
import org.scalatest.{FlatSpec, Matchers}

/**
  * Integration test for the Clu parser
  * User: mihais
  * Date: 7/26/17
  */
class TestCluParser extends FlatSpec with Matchers {
  val procUniversal = new CluProcessor()
  val procStanford = new CluProcessorWithStanford()

  "CluProcessor" should "parse WSJ with an accuracy over 89%" in {
    val model = procUniversal.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/wsj_ud_test.conllu")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"WSJ UD performance: $las, $uas")
    reader.close()
    (las > 0.891) should be (true)
    (uas > 0.907) should be (true)
  }

  it should "parse Genia with an accuracy over 86%" in {
    val model = procUniversal.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/genia_ud_test.conllu")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"Genia UD performance: $las, $uas")
    reader.close()
    (las > 0.865) should be (true)
    (uas > 0.877) should be (true)
  }

  "CluProcessorWithStanford" should "parse WSJ with an accuracy over 89%" in {
    val model = procStanford.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/wsj_test.conllx")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"WSJ SD performance: $las, $uas")
    reader.close()
    (las > 0.891) should be (true)
    (uas > 0.906) should be (true)
  }

  it should "parse Genia with an accuracy over 87%" in {
    val model = procStanford.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/genia_test.conllx")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"Genia SD performance: $las, $uas")
    reader.close()
    (las > 0.874) should be (true)
    (uas > 0.884) should be (true)
  }
}
