package org.clulab.openie.entities

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.openie.ResourceUtils
import org.clulab.odin.{ExtractorEngine, Mention, State, TextBoundMention}
import org.clulab.openie.utils.TagSet
import org.clulab.processors.Document
import org.clulab.struct.Interval

import scala.collection.JavaConverters._
import scala.util.matching.Regex


class CustomizableRuleBasedFinder(
  entityEngine: ExtractorEngine,
  avoidEngine: ExtractorEngine,
  override val tagSet: TagSet,
  stopNER: Set[String],
  ensureBaseTagNounVerb: Boolean,
  maxHops: Int,
  maxLength: Int,
  override val INVALID_OUTGOING: Set[scala.util.matching.Regex],
  override val INVALID_INCOMING: Set[scala.util.matching.Regex],
  override val VALID_OUTGOING: Set[scala.util.matching.Regex]
  ) extends RuleBasedEntityFinder(entityEngine, avoidEngine, maxHops, maxLength) {

  /**
   * Performs rule-based entity extraction with selective expansion along syntactic dependencies.
   * For filtering, see filterEntities.
   * @param doc an org.clulab.processors.Document
   */
  override def extract(doc: Document): Seq[Mention] = {
    // avoid refs, etc.
    val avoid = avoidEngine.extractFrom(doc)
    val stateFromAvoid = State(avoid)
    val baseEntities = entityEngine.extractFrom(doc, stateFromAvoid).filterNot(stateFromAvoid.contains)
    // make sure that all are valid (i.e., contain a noun or would have contained a noun except for trigger avoidance)
    val validBaseEntities = baseEntities.filter(isValidBaseEntity)
    // Expand
    val expandedEntities = validBaseEntities.map(entity => expand(entity, maxHops, stateFromAvoid))
    // split entities on likely coordinations
    val splitEntities = (validBaseEntities ++ expandedEntities).flatMap(splitCoordinatedEntities)
    // remove entity duplicates introduced by splitting expanded
    val distinctEntities = splitEntities.distinct
    // trim unwanted POS from entity edges
    val trimmedEntities = distinctEntities.map(trimEntityEdges(_, tagSet))
    // if there are no avoid mentions or if we didn't expand, no need to filter
    val res = if (avoid.isEmpty || maxHops == 0) {
      trimmedEntities
    } else {
      // check that our expanded entities haven't swallowed any avoid mentions
      val avoidLabel = avoid.head.labels.last
      trimmedEntities.filterNot { m => stateFromAvoid.hasMentionsFor(m.sentence, m.tokenInterval, avoidLabel) }
    }
    res
  }

  /**
   * Determines whether or not an entity is a valid base entity.
   */
  def isValidBaseEntity(entity: Mention): Boolean = {
    val tags = entity.tags.get
    val entities = entity.entities.get

    // Make sure there is a noun that isn't a specifically excluded  named entity.
    tags.indices.exists { i =>
      isValidTag(tags(i)) && !stopNER.contains(entities(i))
    }
  }

  /**
   * Determines if the tag is a noun/verb
   * @param tag the POS tag to consider
   */
  def isValidTag(tag: String): Boolean = {
    !ensureBaseTagNounVerb || tagSet.isAnyNoun(tag) || tagSet.isAnyVerb(tag)
  }


  /**
   * Trims found entities of leading or trailing unwanted tokens.  Currently, we define "unwanted" as being POS tagged
   * with one of the tags in INVALID_EDGE_TAGS.
   * @param entity the candidate entity Mention
   * @return TextBoundMention with valid interval
   */
  def trimEntityEdges(entity: Mention, tagSet: TagSet): Mention = {
    // Check starting tag, get the location of first valid tag
    val tags = entity.document.sentences(entity.sentence).tags.get
    val startToken = entity.tokenInterval.start
    val startTag = tags(startToken)
    val firstValidStart = if (validEdgeTag(startTag, tagSet)) startToken else firstValid(tags, startToken, tagSet)

    // Check ending tag, get the location of last valid tag
    val endToken = entity.tokenInterval.end - 1  // subtracting 1 bc interval is exclusive
    val endTag = tags(endToken)
    val lastValidEnd = if (validEdgeTag(endTag, tagSet)) endToken else lastValid(tags, endToken, tagSet)

    if (firstValidStart == startToken && lastValidEnd == endToken) {
      // No trimming needed because both first and last were valid
      entity
    } else if (firstValidStart > lastValidEnd) {
      // If you trimmed everything...
      entity
    }
    else {
      // Return a new entity with the trimmed token interval
      val interval = Interval(firstValidStart, lastValidEnd + 1)
      entity.asInstanceOf[TextBoundMention].copy(tokenInterval = interval)
    }
  }

  // Find the first valid token in the mention's token interval
  def firstValid(tags: Seq[String], mentionStart: Int, tagSet: TagSet): Int = {
    // As indexWhere returns -1 in the event it doesn't find any, here we add the max to default to the first token
    math.max(tags.indexWhere(tag => validEdgeTag(tag, tagSet), from = mentionStart), 0)
  }

  // Find the last valid token in the mention's token interval
  // mentionEnd is inclusive
  def lastValid(tags: Seq[String], mentionEnd: Int, tagSet: TagSet): Int = {
    // As indexWhere returns -1 in the event it doesn't find any, here we add the max to default to the first token
    // Note: end is inclusive
    math.max(tags.lastIndexWhere(tag => validEdgeTag(tag, tagSet), end = mentionEnd), 0)
  }

  def validEdgeTag(tag: String, tagSet: TagSet): Boolean = !tagSet.isInvalidEdge(tag)

}

object CustomizableRuleBasedFinder {

  def fromConfig(config: Config = ConfigFactory.load()): CustomizableRuleBasedFinder = {
    val entityRulesPath: String = config.getString("CustomRuleBasedEntityFinder.entityRulesPath")
    val entityRules = ResourceUtils.readResource(entityRulesPath)
    val entityEngine = ExtractorEngine(entityRules)

    val avoidRulesPath: String = config.getString("CustomRuleBasedEntityFinder.avoidRulesPath")
    val avoidRules = ResourceUtils.readResource(avoidRulesPath)
    val avoidEngine = ExtractorEngine(avoidRules)

    val tagSet: TagSet = TagSet(config.getString("CustomRuleBasedEntityFinder.language"))
    val stopNER: Set[String] = config.getStringList("CustomRuleBasedEntityFinder.stopNER").asScala.toSet
    val ensureBaseTagNounVerb: Boolean = config.getBoolean("CustomRuleBasedEntityFinder.ensureBaseTagNounVerb")
    val maxHops: Int = config.getInt("CustomRuleBasedEntityFinder.maxHops")
    val maxLength: Int = config.getInt("CustomRuleBasedEntityFinder.maxLength")
    val invalidOutgoing: Set[Regex] = asRegexSet(config.getStringList("CustomRuleBasedEntityFinder.invalidOutgoing").asScala.toSet)
    val invalidIncoming: Set[Regex] = asRegexSet(config.getStringList("CustomRuleBasedEntityFinder.invalidIncoming").asScala.toSet)
    val validOutgoing: Set[Regex] = asRegexSet(config.getStringList("CustomRuleBasedEntityFinder.validOutgoing").asScala.toSet)

    new CustomizableRuleBasedFinder(
      entityEngine,
      avoidEngine,
      tagSet,
      stopNER,
      ensureBaseTagNounVerb,
      maxHops,
      maxLength,
      invalidOutgoing,
      invalidIncoming,
      validOutgoing
    )
  }

  def asRegexSet(ss: Set[String]): Set[Regex] = {
    ss.map{ case s: String => s.r }
  }
}