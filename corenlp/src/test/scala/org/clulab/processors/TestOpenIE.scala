package org.clulab.processors

import java.io.{PrintWriter, StringWriter}

import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.serialization.DocumentSerializer
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class TestOpenIE extends FlatSpec with Matchers {

  private val text = "Obama was born in Hawaii. He is our president."
  private val fastNLP= new FastNLPProcessor(withRelationExtraction = true)
  private val coreNLP = new CoreNLPProcessor(withRelationExtraction = true)
  private val serializer = new DocumentSerializer

  private lazy val fastNLPDoc = fastNLP.annotate(text)
  private lazy val coreNLPDoc = coreNLP.annotate(text)

  private val buffer = new StringWriter()
  serializer.save(fastNLPDoc, new PrintWriter(buffer))
  private val serialized = buffer.toString

  private val deserializedDoc = serializer.load(serialized)

  def openIEBehavior(doc:Document) {
    val relations = doc.sentences.map(_.relations).collect{ case Some(triples) => triples }.flatten

    it should "have relation extraction annotations" in {
      relations should not be empty
    }

    it should "extract three relations" in {
      relations should have length 3
    }

    it should "have the correct entities and relation labels" in {
      val subjects = new mutable.HashSet[String]
      val objects = new mutable.HashSet[String]
      val relations = new mutable.HashSet[String]

      doc.sentences.foreach{
        implicit s =>
          subjects ++= s.relations.get.map(_.subjectText)
          objects ++= s.relations.get.map(_.objectText)
          relations ++= s.relations.get.map(_.relationText)
      }

      subjects should contain ("Obama")
      subjects should contain ("He")
      relations should contain ("was")
      relations should contain ("was born in")
      relations should contain ("is")
      objects  should contain ("born")
      objects  should contain ("Hawaii")
      objects  should contain ("our president")
    }
  }

  "FastNLPProcessor" should behave like openIEBehavior(fastNLPDoc)

  "CoreNLPProcessor" should behave like openIEBehavior(coreNLPDoc)

  "Document object with relation annotations" should "be serialized correctly" in {
    serialized should not be empty
  }

  "Deserialized document" should behave like openIEBehavior(deserializedDoc)

  "Processors" should "not throw a java.lang.IllegalArgumentException" in {
    val text = "North Andover is a town in Essex County, Massachusetts, United States. At the 2010 census the population was 28,352."
    val doc = fastNLP.annotate(text)

    val relations = doc.sentences.map(_.relations).collect{case Some(a) => a}.flatten

    relations should not be empty
  }

  it should "not throw a java.util.NotSuchElementException" in {
    val text = "John Alan Lasseter (born January 12, 1957) is an American animator and film director, who is the chief creative officer of Pixar Animation Studios, Walt Disney Animation Studios, and DisneyToon Studios. He is also the Principal Creative Advisor for Walt Disney Imagineering."
    val doc = fastNLP.annotate(text)

    val relations = doc.sentences.map(_.relations).collect{case Some(a) => a}.flatten

    // For some reason, CoreNLP sometimes omits the relation property of RelationTriple in OpenIE.
    // In such cases, the relation interval takes a None value. and semantically it is an "is a" relation
    relations exists (_.relationInterval.isEmpty) should be (true)

  }
}
