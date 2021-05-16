package org.clulab.openie.conceptdiscovery

import org.json4s.JArray
import org.json4s.JValue
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods

class ConceptSink(scorededConcepts: Seq[ScoredConcept]) {

  def toJValue: JValue = {
    val jArray = new JArray(
      scorededConcepts.toList.map { scoredConcept =>
        ("concept" ->
          ("phrase" -> scoredConcept.concept.phrase) ~
          ("locations" -> new JArray( {
            val documentIdsAndSentenceIndexes = scoredConcept.concept.documentLocations.toList.map { documentLocation =>
              (documentLocation.docid, documentLocation.sent)
            }.sortBy(_._2)
            val jObjects = documentIdsAndSentenceIndexes.map { case (documentId, sentenceIndex) =>
              ("document_id" -> documentId) ~
              ("sentence_index" -> sentenceIndex)
            }

            jObjects
          }))
        ) ~
        ("saliency" -> scoredConcept.saliency)
      }
    )

    jArray
  }

  def printJson(): Unit = {
    val jValue = toJValue
    val json = JsonMethods.pretty(jValue)

    println(json)
  }
}