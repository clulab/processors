package org.clulab.processors

import java.io.{BufferedReader, InputStreamReader}

import org.clulab.processors.clu.PortugueseCluProcessor
import org.clulab.processors.clu.syntax.EvaluateMalt
import org.scalatest.{FlatSpec, Matchers}

class TestPortugueseCluParser extends FlatSpec with Matchers {
  // TODO: see TestCluParser
  val procUniversal = new PortugueseCluProcessor()

  "PortugueseCluProcessor" should "parse UD with an accuracy over 80%" in {
    val model = procUniversal.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/pt_test.conllu")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"PT UD performance: $las, $uas")
    reader.close()
    (las > 0.8) should be (true)
    (uas > 0.8) should be (true)
  }
}
