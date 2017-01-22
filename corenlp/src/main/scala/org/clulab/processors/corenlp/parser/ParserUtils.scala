package org.clulab.processors.corenlp.parser

import java.io.File
import java.util.Properties

import com.typesafe.config.ConfigFactory
import edu.stanford.nlp.parser.nndep.DependencyParser
import org.clulab.processors.Sentence
import org.clulab.struct.{Tree => _, _}
import org.clulab.utils.ConllxReader


object ParserUtils {
  def copyWithoutDependencies(s: Sentence): Sentence = s.copy(dependenciesByType = new GraphMap)
}

object TrainDependencyParser extends App {

  if (args.length < 2 ||
    !(args(0) == "wsj" || args(0) == "genia" || args(0) == "wsj-genia") ||
    !(args(1) == "w2v" || args(1) == "pmc" || args(1) == "w2v-pmc")) {
    println("Usage: TrainDependencyParser [wsj|genia|wsj-genia] [w2v|pmc|w2v-pmc] [1|2|3|4|5]")
    System.exit(1)
  }

  val config = ConfigFactory.load()

  // wsj w2v
  var language = config.getString("corenlp.language")
  var tf = config.getString("corenlp.parser.wsj.trainFile")
  var df = config.getString("corenlp.parser.wsj.devFile")
  var ef = config.getString("corenlp.parser.w2v.embeddings")
  var es = config.getString("corenlp.parser.w2v.embeddingsDim")
  var mf = config.getString("corenlp.parser.wsj.model")

  if (args(0) == "genia") {
    tf = config.getString("corenlp.parser.genia.trainFile")
    df = config.getString("corenlp.parser.genia.devFile")
    mf = config.getString("corenlp.parser.genia.model")
  }

  if (args(0) == "wsj-genia") {
    val k = args(2)
    assert(k == "1" || k == "2" || k == "3" || k == "4" || k == "5")
    tf = config.getString(s"corenlp.parser.wsj-genia.${k}.trainFile")
    df = config.getString(s"corenlp.parser.wsj-genia.${k}.devFile")
    mf = config.getString(s"corenlp.parser.wsj-genia.${k}.model")
  }

  if (args(1) == "pmc") {
    ef = config.getString("corenlp.parser.pmc.embeddings")
    es = config.getString("corenlp.parser.pmc.embeddingsDim")
  }

  if (args(1) == "w2v-pmc") {
    ef = config.getString("corenlp.parser.w2v-pmc.embeddings")
    es = config.getString("corenlp.parser.w2v-pmc.embeddingsDim")
  }

  // prepare dependency parser
  val props = new Properties()
  props.put("language", language)
  props.put("trainFile", tf)
  props.put("embedFile", ef)
  props.put("embeddingSize", es)
  //  println(s"embeddingSize will be retrieved as '${PropertiesUtils.getInt(props, "embeddingSize")}'")
  props.put("model", mf)
  println(s"props: $props")

  val dep = new DependencyParser(props)

  // train parser
  dep.train(tf, df, mf, ef, null)
}

object TestDependencyParser extends App {
  if (args.length < 2 ||
    !(args(0) == "wsj" || args(0) == "genia" || args(0) == "wsj-genia")) {
    println("Usage: TestDependencyParser [wsj|genia] [wsj|genia|wsj-genia] [1|2|3|4|5]")
    System.exit(1)
  }

  val config = ConfigFactory.load()

  // wsj wsj
  var model = config.getString("corenlp.parser.wsj.model")
  var testFile = config.getString("corenlp.parser.wsj.testFile")

  if (args(0) == "genia")
    testFile = config.getString("corenlp.parser.genia.testFile")

  if (args(1) == "genia")
    model = config.getString("corenlp.parser.genia.model")

  if (args(1) == "wsj-genia") {
    val k = args(2)
    assert(k == "1" || k == "2" || k == "3" || k == "4" || k == "5")
    model = config.getString(s"corenlp.parser.wsj-genia.${k}.model")
  }

  // prepare dependency parser
  val props = new Properties()
  val dep = new DependencyParser(props)
  dep.loadModelFile(model)
  println("Loaded model!")

  val outFile = "tmp.conllx"
  dep.testCoNLL(testFile, outFile)

  val doc = ConllxReader.load(new File(testFile))
  val copy = ConllxReader.load(new File(outFile))

  var results = EvaluateUtils.Performance(0,0,0,0,"testDependencyParser")
  for (i <- doc.sentences.indices) {
    results += EvaluateUtils.evaluate(
      doc.sentences(i).dependenciesByType(GraphMap.STANFORD_BASIC),
      copy.sentences(i).dependenciesByType(GraphMap.STANFORD_BASIC),
      withEdgeLabel = true
    )
  }
  printResults(results)

  new File(outFile).deleteOnExit()

  private def printResults(results: EvaluateUtils.Performance): Unit = {
    println(s"Results for ${results.label}:")
    println(s"  tp: ${results.tp}, fp: ${results.fp}, tn: ${results.tn}, fn: ${results.fn}")
    println(s"  Precision: ${results.precision}")
    println(s"  Recall: ${results.recall}")
    println(s"  F1: ${results.f1}")
  }
}
