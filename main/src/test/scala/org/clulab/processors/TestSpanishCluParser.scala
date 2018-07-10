package org.clulab.processors

import java.io.{BufferedReader, InputStreamReader}

import org.clulab.processors.clu.SpanishCluProcessor
import org.clulab.processors.clu.syntax.EvaluateMalt
import org.scalatest.{FlatSpec, Matchers}

class TestSpanishCluParser extends FlatSpec with Matchers {
  // TODO: see TestCluParser
  val procUniversal = new SpanishCluProcessor()

  "SpanishCluProcessor" should "parse UD with an accuracy over 80%" in {
    val model = procUniversal.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/es_test.conllu")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"ES UD performance: $las, $uas")
    reader.close()
    (las > 0.8) should be (true)
    (uas > 0.8) should be (true)
  }
}
