package org.clulab.processors

import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.naturalli.NaturalLogicAnnotations.RelationTriplesAnnotation
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.struct.{Interval, RelationTriple}

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

trait OpenIEAnnotator {

  // Self-typing because every class mixining this trait is going to be a descendant of ShallowNLPProcessor
  this:ShallowNLPProcessor =>

  def mkOpenIE: StanfordCoreNLP ={
    val props = new Properties()
    props.put("annotators", "natlog,openie")
    newStanfordCoreNLP(props, enforceRequirements = false)
  }

  lazy val openIE: StanfordCoreNLP = mkOpenIE

    def relationExtractionSanityCheck(doc:Document):Option[Annotation] = {
      if(withRelationExtraction) {
        val annotation = basicSanityCheck(doc)
        if (annotation.isEmpty) return None
        if (doc.sentences.head.tags.isEmpty)
          throw new RuntimeException("ERROR: you have to run the POS tagger before OpenIE!")
        if (doc.sentences.head.lemmas.isEmpty)
          throw new RuntimeException("ERROR: you have to run the lemmatizer before OpenIE!")
        if (doc.sentences.head.dependencies.isEmpty)
          throw new RuntimeException("ERROR: you have to run the dependency parser before OpenIE!")
        annotation
      }
      else
        None
    }

  override def relationExtraction(doc: Document) {
    val annotation = relationExtractionSanityCheck(doc)
    if(annotation.isEmpty) return

    try{
      openIE.annotate(annotation.get)
    } catch {
      case e:Exception =>
        println("Caught OpenIE exception!")
        println("Document:\n" + doc)
        throw e
    }

    // convert CoreNLP Annotations to our data structures
    val sas = annotation.get.get(classOf[SentencesAnnotation]).asScala
    var offset = 0
    for (sa <- sas) {
      val relationInstances = new ArrayBuffer[RelationTriple]
      val triplets = sa.get(classOf[RelationTriplesAnnotation]).asScala
      for (triplet <- triplets) {
        val confidence = triplet.confidence
        val canonicalSubject = triplet.canonicalSubject.asScala
        val subjectStart = canonicalSubject.head.index - 1
        val subjectEnd = subjectStart + canonicalSubject.length

        val relationInterval = triplet.relation.asScala match {
          case b if b.nonEmpty =>
            val relationStart = b.head.index - 1
            val relationEnd = relationStart + b.length
            Some(Interval(relationStart, relationEnd))
          case _ =>
            None
        }

        val canonicalObject = triplet.canonicalObject.asScala
        val objectStart = canonicalObject.head.index - 1
        val objectEnd= objectStart + canonicalObject.length

        val relation = RelationTriple(confidence.toFloat, Interval(subjectStart, subjectEnd),relationInterval , Interval(objectStart, objectEnd))
        relationInstances += relation
      }

      doc.sentences(offset).relations = Some(relationInstances.toArray)
      offset += 1
    }
  }
}
