package org.clulab.ie

import org.clulab.ie.entities.RuleBasedEntityFinder
import org.clulab.odin.{ ExtractorEngine, Mention, State }
import org.clulab.processors.Document
import org.clulab.odin.serialization.json.MentionOps
//import org.clulab.odin.serialization.json.MentionOps


/**
  * System for extracting influence relations from [[org.clulab.processors.Document]]
  */
class InfluencerSystem {

  val rules = Utils.readResource("org/clulab/grammar/rules.yml")
  val actions = new InfluenceActions
  val engine = ExtractorEngine(rules = rules, actions = actions, globalAction = actions.analyzePolarity)

  val entityFinder: RuleBasedEntityFinder = RuleBasedEntityFinder(maxHops = 3)

  def extractFrom(doc: Document): Vector[Mention]  = {
    val entities = entityFinder.extractAndFilter(doc)
    val state = State(entities)
    // events, etc.
    val res = engine.extractFrom(doc, state)
    // filter results
    val filteredResults = filterEvents(res)
    filteredResults.toVector
  }

  /** Filter out some events */
  def filterEvents(mentions: Seq[Mention]): Seq[Mention] = {
    // disallow trigger overlaps
    val mns1 = InfluenceActions.disallowTriggerOverlap(mentions, new State())
    mns1
  }

  def extractMentionsWithMetaData(doc: Document): ExtractionResults = {
    val res = for {
      m <- extractFrom(doc)
      metadata = getMetaData(m)
    } yield (m, metadata)
    ExtractionResults(res)
  }

  def getMetaData(mention: Mention): MentionMetaData = {
    val negated = Negation.isNegated(mention)
    val hedged = Hedging.isHedged(mention)
    MentionMetaData(mention.id, negated, hedged)
  }

}
