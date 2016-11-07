package org.clulab.processors.corenlp.parser

import java.io.Reader
import java.util.Properties
import com.typesafe.config.ConfigFactory
import edu.stanford.nlp.parser.nndep.DependencyParser
import edu.stanford.nlp.trees._
import scala.collection.JavaConverters._


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

// FIXME: WIP
// adapted from http://www-nlp.stanford.edu/software/TypedDependenciesDemo.java
//java -cp stanford-parser.jar edu.stanford.nlp.trees.EnglishGrammaticalStructure -treeFile wsj/02/wsj_0201.mrg -basic
object TreebankConverter extends App {
  //val config = ConfigFactory.load()
  //val treeBank = config.getString("corenlp.parser.treeBank")
  // parser FAQ: http://www-nlp.stanford.edu/software/parser-faq.shtml

  val tb = new DiskTreebank(new TreeReaderFactory() {
    def newTreeReader(in: Reader): TreeReader = {
      new PennTreeReader(in, new LabeledScoredTreeFactory(),
        new NPTmpRetainingTreeNormalizer())
    }
  })

  tb.loadPath(args(0))
  val tlp: TreebankLanguagePack = new PennTreebankLanguagePack()
  val gsf: GrammaticalStructureFactory = tlp.grammaticalStructureFactory()
  val tp = new TreePrint("typedDependenciesBasic")

  for (t: Tree <- tb.iterator().asScala.toSeq) {
    val gs: GrammaticalStructure = gsf.newGrammaticalStructure(t)
    System.out.println(gs.typedDependenciesCollapsed())
    System.out.println()
    tp.printTree(t)
    System.out.println("----------")
  }
}
