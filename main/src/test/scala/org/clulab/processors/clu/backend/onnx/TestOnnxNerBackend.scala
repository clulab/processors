package org.clulab.processors.clu.backend.onnx

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.clu.AnnotatedSentence
import org.clulab.processors.clu.backend.OnnxNerBackend
import org.clulab.utils.{Configured, Test}

class TestOnnxNerBackend extends Test with Configured {
  val config = ConfigFactory.load("cluprocessor")

  override def getConf: Config = config

  val prefix:String = "CluProcessor"
  val nerBackend = new OnnxNerBackend(
    getArgString(s"$prefix.onnx.ner.wordModel", Some("/org/clulab/processors/clu/onnx/ner/word.onnx")),
    getArgString(s"$prefix.onnx.ner.charModel", Some("/org/clulab/processors/clu/onnx/ner/char.onnx")),
    getArgString(s"$prefix.onnx.ner.x2iMapper", Some("/org/clulab/processors/clu/onnx/ner/x2i.json"))
  )
  val words = "Published on Famine Early Warning Systems Network ( http://www.fews.net ) East Africa South Sudan This country is monitored by local FEWS NET staff Key Message Update Crisis ( IPC Phase 3 ) and Emergency ( IPC Phase 4 ) persist as the lean season starts early January 2018 Key Messages : Crisis ( IPC Phase 3 ) and Emergency ( IPC Phase 4 ) outcomes persist in all regions of South Sudan in January .".split(" ")
  val posTags = "VBN IN NNP NNP NNP NNPS NNP -LRB- JJ -RRB- NNP NNP NNP NNP DT NN VBZ VBN IN JJ NNP NNP NN NNP NNP NNP NNP -LRB- NNP NNP CD -RRB- CC NNP -LRB- NNP NN CD -RRB- VBP IN DT JJ NN VBZ JJ NNP CD NNP NNP : NNP -LRB- NNP NN CD -RRB- CC NN -LRB- NNP NN CD -RRB- NNS VBP IN DT NNS IN NNP NNP IN NNP .".split(" ")
  val annotatedSentence = AnnotatedSentence(words, Some(posTags))
  val expectedNers = nerBackend.predict(annotatedSentence, null).mkString(" ")
  var count = 0

  while ({
    val actualNers = nerBackend.predict(annotatedSentence, null).mkString(" ")

    count += 1
    if (actualNers != expectedNers) {
      println(s"$count is bad!")
      true
    }
    else {
      println(s"$count is good.")
      true
    }
  }) ()
}
