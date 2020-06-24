package org.clulab.processors

import java.util.Properties

import edu.stanford.nlp.ie.NERClassifierCombiner
import edu.stanford.nlp.ie.NERClassifierCombiner
import edu.stanford.nlp.pipeline.Annotator
import edu.stanford.nlp.pipeline.AnnotatorImplementations
import edu.stanford.nlp.pipeline.NERCombinerAnnotator
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.util.MutableLong
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.scalatest._

import scala.io.Source

class TestFastNLPProcessorEnv extends FlatSpec with Matchers {

  class TestableNERCombinerAnnotator(ner: NERClassifierCombiner, verbose: Boolean) extends NERCombinerAnnotator(ner, verbose) {
    val myNer = ner

    def this() = this(new NERClassifierCombiner(new Properties()), true)
  }

  class TestableStanfordCoreNLP(props: Properties, enforceRequirements: Boolean = true)
      extends StanfordCoreNLP(props, enforceRequirements) {
//    val myAnnotatorPool = annotatorPool
    println("Got here!")
    var nerCombinerAnnotatorOpt: Option[NERCombinerAnnotator] = if (nerCombinerAnnotatorOpt == null) None else nerCombinerAnnotatorOpt

    override def addAnnotator(annotator: Annotator): Unit = {
      val newAnnotator =
        if (annotator.isInstanceOf[NERCombinerAnnotator]) {
          val newAnnotator = new TestableNERCombinerAnnotator
          nerCombinerAnnotatorOpt = Option(newAnnotator)
          newAnnotator
        }
        else annotator
      super.addAnnotator(newAnnotator)
    }

    override protected def getAnnotatorImplementations(): AnnotatorImplementations = {
      val annotatorImplementations = super.getAnnotatorImplementations()

      annotatorImplementations
    }
  }

  class TestableFastNLPProcessor extends FastNLPProcessor() {
    val myNer = ner // Do away with laziness.
    var testableStanfordCoreNLPOpt: Option[TestableStanfordCoreNLP] = None

    override protected def newStanfordCoreNLP(props: Properties, enforceRequirements: Boolean = true): StanfordCoreNLP = {
      // Prevent knownLCWords from changing on us.  To be safe, this is added every time
      // because of potential caching of annotators.  Yes, the 0 must be a string.
      props.put("maxAdditionalKnownLCWords", "0")
      val result = new TestableStanfordCoreNLP(props, enforceRequirements)

      testableStanfordCoreNLPOpt = Option(result)
      result
    }

    override def annotate(doc: Document): Document = {
      tagPartsOfSpeech(doc)
      lemmatize(doc)
      recognizeNamedEntities(doc)
      parse(doc)
      // That's enough to show error.
      doc
    }
  }

  def mkDocument(text: String, date: String): Document = {
    val tokenizedDoc = proc.mkDocument(text, keepText = true)

    tokenizedDoc.setDCT(date)
    tokenizedDoc
  }

  def annotate(tokenizedDoc: Document): String = {
    val annotatedDoc: Document = proc.annotate(tokenizedDoc)
    val norms = annotatedDoc.sentences.flatMap(_.norms.get).mkString("\n")

    norms
  }

  var proc: TestableFastNLPProcessor = new TestableFastNLPProcessor()

  "FastNLPProcessor" should "get the same answer when reannotating a document" in {
      val inputDir = "./corenlp/src/test/resources/documents"

      val text1 = Source.fromFile(inputDir + "/1742.txt", "UTF-8").mkString // unclosed
      val text2 = Source.fromFile(inputDir + "/73f3.txt", "UTF-8").mkString // unclosed

      val date1 = "2012-09-22"
      val date2 = "2010-06-08"

      val tokenizedDoc1a = mkDocument(text1, date1)
      val tokenizedDoc1b = mkDocument(text1, date1)
      val tokenizedDoc2  = mkDocument(text2, date2)

      val expected = annotate(tokenizedDoc1a)
      annotate(tokenizedDoc2)
      val actual = annotate(tokenizedDoc1b)

      expected should be (actual)
  }
}
