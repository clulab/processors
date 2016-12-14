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

  val config = ConfigFactory.load()

  val language = config.getString("corenlp.language")
  val tf = config.getString("corenlp.parser.wsj.trainFile")
  val df = config.getString("corenlp.parser.wsj.devFile")
  val ef = config.getString("corenlp.parser.w2v.embeddings")
  val es = config.getString("corenlp.parser.w2v.embeddingsDim")
  val mf = config.getString("corenlp.parser.wsj.model")

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
  dep.loadModelFile("/net/kate/storage/work/tishihara/en-bio-dep-genia-parser.model.txt.gz")
  println("Loaded model!")

  import org.clulab.processors.fastnlp.FastNLPProcessor
  val proc = new FastNLPProcessor(withChunks = false)
  val doc = proc.annotate("My name is Terron.")
  println(s"Num of sentences: ${doc.sentences.length}")

  import org.clulab.processors.corenlp.CoreNLPUtils
  val cm = CoreNLPUtils.sentenceToCoreMap(doc.sentences.head)
  val gs = dep.predict(cm)
  println(s"${gs}")

  // train parser
//  dep.train(tf, df, mf, ef, null)
}
