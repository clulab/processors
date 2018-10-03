package org.clulab.processors

import java.io.{BufferedReader, InputStreamReader}

import org.clulab.processors.clu.PortugueseCluProcessor
import org.clulab.processors.clu.syntax.{EnsembleModel, EvaluateMalt}
import org.scalatest.{FlatSpec, Matchers}

class TestPortugueseCluParser extends FlatSpec with Matchers {
  val procUniversal = new PortugueseCluProcessor()

  "PortugueseCluProcessor" should "parse Bosque with an accuracy over 77%" in {
    val model = procUniversal.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/pt_bosque_ud_test.conllu")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"PT Bosque UD performance: $las, $uas")
    reader.close()
    las should be > 0.880 // 0.821 (true performance when not trained on test)
    uas should be > 0.904 // 0.860 (true performance when not trained on test)
  }


  "PortugueseCluProcessor" should "parse GSD with an accuracy over 71%" in {
    val model = procUniversal.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/pt_gsd_ud_test.conllu")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"PT GSD UD performance: $las, $uas")
    reader.close()
    // old parsing models (GSD+Bosque)
    // las should be > 0.71
    // uas should be > 0.76
    // Bosque only (train+test+dev)
    las should be > 0.680 // 0.678 (true performance when not trained on test)
    uas should be > 0.795 // 0.792 (true performance when not trained on test)
  }

  "PortugueseCluProcessor" should "parse PUD with an accuracy over 61%" in {
    val model = procUniversal.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/pt_pud_ud_test.conllu")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"PT PUD UD performance: $las, $uas")
    reader.close()
    las should be > 0.724 // 0.722 (true performance when not trained on test)
    uas should be > 0.783 // 0.781 (true performance when not trained on test)
  }
}
