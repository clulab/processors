package org.clulab.openie.conceptdiscovery

import org.scalatest.{FlatSpec, Matchers}

class TestConceptDiscoverer extends FlatSpec with Matchers {

  behavior of "ConceptDiscoverer"

  // Texts from https://en.wikipedia.org/wiki/Food_security
  val texts = Seq(
    Seq(
      ("Food security is a measure of the availability of food and individuals' ability to access it.", 0.4),
      ("According the United Nations’ Committee on World Food Security, food security is defined as the means that all people, at all times, have physical, social, and economic access to sufficient, safe, and nutritious food that meets their food preferences and dietary needs for an active and healthy life.[1]", 0.6),
      ("The availability of food irrespective of class, gender or region is another one.", 0.2),
      ("There is evidence of food security being a concern many thousands of years ago, with central authorities in ancient China and ancient Egypt being known to release food from storage in times of famine.", 0.2),
      ("""At the 1974 World Food Conference the term "food security" was defined with an emphasis on supply; food security is defined as the "availability at all times of adequate, nourishing, diverse, balanced and moderate world food supplies of basic foodstuffs to sustain a steady expansion of food consumption and to offset fluctuations in production and prices".[2]""", 0.2),
      ("Later definitions added demand and access issues to the definition.", 0.2),
      ("""The final report of the 1996 World Food Summit states that food security "exists when all people, at all times, have physical and economic access to sufficient, safe and nutritious food to meet their dietary needs and food preferences for an active and healthy life."[3][4]""", 0.2)),
    Seq(
      ("Household food security exists when all members, at all times, have access to enough food for an active, healthy life.[5]", 0.4),
      ("Individuals who are food secure do not live in hunger or fear of starvation.[6]", 0.6),
      ("""Food insecurity, on the other hand, is defined by the United States Department of Agriculture (USDA) as a situation of "limited or uncertain availability of nutritionally adequate and safe foods or limited or uncertain ability to acquire acceptable foods in socially acceptable ways".[7]""", 0.2),
      ("Food security incorporates a measure of resilience to future disruption or unavailability of critical food supply due to various risk factors including droughts, shipping disruptions, fuel shortages, economic instability, and wars.", 0.2),
      ("In the years 2011–2013, an estimated 842 million people were suffering from chronic hunger.[8]", 0.2),
      ("The Food and Agriculture Organization of the United Nations, or FAO, identified the four pillars of food security as availability, access, utilization, and stability.[9]", 0.2),
      ("The United Nations (UN) recognized the Right to Food in the Declaration of Human Rights in 1948,[6] and has since said that it is vital for the enjoyment of all other rights.[10]", 0.2)))
  // convert texts to CDR documents
  val documents = for ((sentencesWithScores, i) <- texts.zipWithIndex) yield {
    var end = 0
    val scoredSentences = for ((sentence, sentenceScore) <- sentencesWithScores) yield {
      val start = end
      end = start + sentence.length
      ScoredSentence(sentence, start, end, sentenceScore)
    }
    DiscoveryDocument(s"doc$i", scoredSentences)
  }

  val conceptDiscovery = ConceptDiscoverer.fromConfig()
  val concepts = conceptDiscovery.discoverConcepts(documents)
  println()

  it should "find food security concepts" in {
    concepts.map(_.phrase) should contain allOf("food security", "access", "availability")
  }

  it should "have reasonable frequency estimates" in {
    concepts.foreach{
      case c @ Concept("food security", _) => c.frequency should be > 4 // actual: 7
      case c @ Concept("access", _) => c.frequency should be >= 2 // actual: 2
      case c @ Concept("availability", _) => c.frequency should be > 2 // actual: 4
      case _ =>
    }
  }

  it should "filter low saliency sentences" in {
    // keeping only most salient 20% of the sentences
    val filteredConcepts = conceptDiscovery.discoverConcepts(documents, Some(0.1)).map(_.phrase)
    filteredConcepts should contain ("food security")
    filteredConcepts should not contain ("availability")
  }

  it should "keep top k by frequency" in {
    val freqEnough = conceptDiscovery.discoverMostFrequentConcepts(documents, None, 1, 2)
    val mostFreq = conceptDiscovery.discoverMostFrequentConcepts(documents, None, 4, 1)
    freqEnough.map(_.phrase) should contain inOrderOnly ("food security", "times")
    mostFreq.map(_.phrase) should contain only ("food security")
  }

  it should "filter URLs" in {
    val urlTests = Seq(
      Seq(
        "www.google.com",
        "www.google",
        "google.com",
        "Mr.google")
    )
    val urlDocuments = urlTests.zipWithIndex.map{ case (sentences, i) =>
      var end = 0
      val scoredSentences = for (sentence <- sentences) yield {
        val start = end
        end = start + sentence.length
        ScoredSentence(sentence, start, end, 1.0)
      }
      // all sentences have equal score
      DiscoveryDocument(s"doc$i", scoredSentences)
    }
    val allRankedConcepts = conceptDiscovery.rankConcepts(conceptDiscovery.discoverConcepts(urlDocuments), 0.0)
    allRankedConcepts.map(_.concept.phrase) should be (Seq("Mr.google"))
  }

}
