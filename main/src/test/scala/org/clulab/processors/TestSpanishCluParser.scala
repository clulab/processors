package org.clulab.processors

import java.io.{BufferedReader, InputStreamReader}

import org.clulab.processors.clu.SpanishCluProcessor
import org.clulab.processors.clu.syntax.EvaluateMalt
import org.scalatest.{FlatSpec, Matchers}

class TestSpanishCluParser extends FlatSpec with Matchers {
  val procUniversal = new SpanishCluProcessor()

  "SpanishCluProcessor" should "parse AnCora with an accuracy over 74%" in {
    val model = procUniversal.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/es_ancora_ud_test.conllu")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"ES AnCora UD performance: $las, $uas")
    reader.close()
    las should be > 0.74
    uas should be > 0.77
  }

  "SpanishCluProcessor" should "parse GSD with an accuracy over 73%" in {
    val model = procUniversal.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/es_gsd_ud_test.conllu")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"ES GSD UD performance: $las, $uas")
    reader.close()
    las should be > 0.73
    uas should be > 0.76
  }

  "SpanishCluProcessor" should "parse PUD with an accuracy over 63%" in {
    val model = procUniversal.depParser
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/es_pud_ud_test.conllu")
    val reader = new BufferedReader(new InputStreamReader(stream))
    val (las, uas) = EvaluateMalt.evaluate(model, reader)
    println(s"ES PUD UD performance: $las, $uas")
    reader.close()
    las should be > 0.63
    uas should be > 0.70
  }

}
