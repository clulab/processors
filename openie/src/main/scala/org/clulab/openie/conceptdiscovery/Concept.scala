package org.clulab.openie.conceptdiscovery


case class Concept(phrase: String, documentLocations: Set[DocumentLocation]) {
  def frequency: Int = documentLocations.size
}

case class DocumentLocation(docid: String, sent: Int)
case class ScoredConcept(concept: Concept, saliency: Double)
