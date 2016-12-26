package org.clulab.processors.corenlp.parser

import java.util.Properties
import com.typesafe.config.ConfigFactory
import edu.stanford.nlp.parser.nndep.DependencyParser
import org.clulab.processors.Sentence
import org.clulab.struct.{Tree => _, _}


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

//  import java.io.File

  val dep = new DependencyParser(props)
//  dep.loadModelFile("/net/kate/storage/work/tishihara/en-bio-dep-genia-parser.model.txt.gz")
//  println("Loaded model!")
//
//  import org.clulab.processors.fastnlp.FastNLPProcessor
//  val proc = new FastNLPProcessor(withChunks = false)
//  val doc = proc.annotate("My name is Terron.")
//  println(s"Num of sentences: ${doc.sentences.length}")
//
//  import org.clulab.processors.corenlp.CoreNLPUtils
//  val cm = CoreNLPUtils.sentenceToCoreMap(doc.sentences.head)
//  val gs = dep.predict(cm)
//  println(s"${gs}")

  // train parser
  dep.train(tf, df, mf, ef, null)
}
