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
  val tf = config.getString("corenlp.parser.trainFile")
  val df = config.getString("corenlp.parser.devFile")
  val ef = config.getString("corenlp.parser.embeddings")
  val es = config.getString("corenlp.parser.embeddingsDim")
  val mf = config.getString("corenlp.parser.model")

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
