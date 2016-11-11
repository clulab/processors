package org.clulab.processors.corenlp.parser

import java.io.{BufferedReader, Reader}
import java.util.Properties

import com.typesafe.config.ConfigFactory
import edu.stanford.nlp.parser.nndep.DependencyParser
import edu.stanford.nlp.trees._
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.{GraphMap, MutableNumber, Tree}

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag


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

// adapted from main/org.clulab.serialization.DocumentSerializer
class ConllxReader {
  val NIL = "_"
  val SEP = "\t"
  val NUM_COLS = 10

  def load(r:BufferedReader): Document = {
    var line:String = null
    val sents = new ArrayBuffer[Sentence]
    var sent:Sentence = null

    while ((sent = loadSentence(r)) != null) {
      sents += sent
    }

    val doc = Document(sents.toArray)
    doc
  }

  private def loadSentence(r:BufferedReader): Sentence = {
    // Ensure there is something to read
    if (r.readLine == null) return null
    r.reset // resets to most recent mark, not the beginning

    val wordBuffer = new ArrayBuffer[String]
    val startOffsetBuffer = new ArrayBuffer[Int]
    val endOffsetBuffer = new ArrayBuffer[Int]
    val tagBuffer = new ArrayBuffer[String]
    var nilTags = true
    val lemmaBuffer = new ArrayBuffer[String]
    var nilLemmas = true

    var line:String = null
    var tokenCount = 0
    while((line = r.readLine) != null && line != "") {
      val bits = line.split(SEP)

      if(bits.length != NUM_COLS) {
        throw new RuntimeException("ERROR: invalid line: " + bits.mkString(" "))
      }

      assert(bits.length == NUM_COLS)
      startOffsetBuffer += bits(0).toInt - 1
      endOffsetBuffer += bits(6).toInt - 1 // may end up being -1 (root), which should be ignored
      wordBuffer += bits(1)
      lemmaBuffer += bits(2)
      if (bits(2) != NIL) nilLemmas = false
      tagBuffer += bits(4) // fine-grained POS tag (coarse-grained is column 3)
      if (bits(4) != NIL) nilTags = false

      tokenCount += 1
    }

    assert(wordBuffer.size == tokenCount)
    assert(startOffsetBuffer.size == tokenCount)
    assert(endOffsetBuffer.size == tokenCount)
    assert(tagBuffer.isEmpty || tagBuffer.size == tokenCount)
    assert(lemmaBuffer.isEmpty || lemmaBuffer.size == tokenCount)

    var deps = new GraphMap
    var tree:Option[Tree] = None
    // FIXME: build dependency tree
//    do {
//      bits = read(r)
//      if (bits(0) == START_DEPENDENCIES) {
//        val dt = bits(1)
//        val sz = bits(2).toInt
//        val d = loadDependencies(r, sz)
//        deps += (dt -> d)
//      } else if (bits(0) == START_CONSTITUENTS) {
//        val position = new MutableNumber[Int](0)
//        bits = read(r)
//        tree = Some(loadTree(bits, position))
//      }
//    } while(bits(0) != END_OF_SENTENCE)

    Sentence(
      wordBuffer.toArray,
      startOffsetBuffer.toArray,
      endOffsetBuffer.toArray,
      bufferOption(tagBuffer, nilTags),
      bufferOption(lemmaBuffer, nilLemmas),
      None,
      None,
      None,
      tree, deps
    )
  }

  private def bufferOption[T: ClassTag](b:ArrayBuffer[T], allNils:Boolean): Option[Array[T]] = {
    if (b.isEmpty) return None
    if (allNils) return None
    Some(b.toArray)
  }

}