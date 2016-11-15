package org.clulab.processors.corenlp.parser

import java.io.{BufferedReader, Reader}
import java.util.Properties

import com.typesafe.config.ConfigFactory
import edu.stanford.nlp.parser.nndep.DependencyParser
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.trees._
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.{Tree => _, _}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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
  val READ_AHEAD_LIMIT = 300

  val ID = 0
  val FORM = 1
  val LEMMA = 2
  val CPOSTAG = 3
  val POSTAG = 4
  val FEATS = 5
  val HEAD = 6
  val DEPREL = 7
  val PHEAD = 8
  val PDEPREL = 9

  def load(r:BufferedReader): Document = {
    var line:String = null
    val sents = new ArrayBuffer[Sentence]
    var sent:Sentence = null

    val textBuffer = new ArrayBuffer[String]

    while ({sent = loadSentence(r); sent != null}) {
      sents += sent
      textBuffer ++= sent.words
    }

    r.close()

    val annotation = new Annotation(textBuffer.mkString(" "))

    val doc = CoreNLPDocument(sents.toArray)
    doc.annotation = Some(annotation)
    doc
  }

  private def loadSentence(r:BufferedReader): Sentence = {
    // Ensure there is something to read
    r.mark(READ_AHEAD_LIMIT)
    if (r.readLine == null) return null
    r.reset

    val wordBuffer = new ArrayBuffer[String]
    val startOffsetBuffer = new ArrayBuffer[Int]
    val endOffsetBuffer = new ArrayBuffer[Int]
    val tagBuffer = new ArrayBuffer[String]
    var nilTags = true
    val lemmaBuffer = new ArrayBuffer[String]
    var nilLemmas = true

    var line:String = null
    var tokenCount = 0

    var deps = new GraphMap
    val edges = new ListBuffer[Edge[String]]
    val roots = new mutable.HashSet[Int]()

    var offset = 0

    while({line = r.readLine; line != null && line != ""}) {
      val bits = line.split(SEP)

      if(bits.length != NUM_COLS) {
        throw new RuntimeException("ERROR: invalid line: " + bits.mkString(" "))
      }

      assert(bits.length == NUM_COLS)

      startOffsetBuffer += offset
      offset += bits(FORM).length + 1 // include a space
      endOffsetBuffer += offset - 1

      wordBuffer += bits(FORM)
      lemmaBuffer += bits(LEMMA)
      if (bits(LEMMA) != NIL) nilLemmas = false
      tagBuffer += bits(POSTAG) // fine-grained POS tag (coarse-grained is column 3)
      if (bits(POSTAG) != NIL) nilTags = false

      // Only create edges for non-roots
      if (bits(HEAD).toInt != 0) {
        val edge = Edge(source = bits(HEAD).toInt-1, destination = bits(ID).toInt-1, relation = bits(DEPREL))
        println("adding edge: " + edge)
        edges += edge
      }
      else roots.add(bits(ID).toInt-1)

      tokenCount += 1
    }

    assert(wordBuffer.size == tokenCount)
    assert(startOffsetBuffer.size == tokenCount)
    assert(endOffsetBuffer.size == tokenCount)
    assert(tagBuffer.isEmpty || tagBuffer.size == tokenCount)
    assert(lemmaBuffer.isEmpty || lemmaBuffer.size == tokenCount)

    val dg = new DirectedGraph[String](edges.toList, roots.toSet)
    deps += (GraphMap.STANFORD_BASIC -> dg)
    //println(dg)

    Sentence(
      wordBuffer.toArray,
      startOffsetBuffer.toArray,
      endOffsetBuffer.toArray,
      bufferOption(tagBuffer, nilTags),
      bufferOption(lemmaBuffer, nilLemmas),
      None, // entities
      None, // norms
      None, // chunks
      None, // tree
      deps
    )
  }

  private def bufferOption[T: ClassTag](b:ArrayBuffer[T], allNils:Boolean): Option[Array[T]] = {
    if (b.isEmpty) return None
    if (allNils) return None
    Some(b.toArray)
  }

}