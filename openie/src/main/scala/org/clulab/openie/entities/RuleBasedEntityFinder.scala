package org.clulab.openie.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.openie.ResourceUtils
import org.clulab.odin.{ ExtractorEngine, Mention, State, TextBoundMention }
import org.clulab.processors.{ Document, Sentence }
import org.clulab.struct.Interval

import scala.annotation.tailrec


/**
  * Finds Open IE-style entities from a [[org.clulab.processors.Document]].
  *
  * @param entityEngine an ExtractorEngine for entities.  Runs AFTER avoidEngine.
  * @param avoidEngine an ExtractorEngine for tokens/spans to be avoided. Runs BEFORE entityEngine.
  * @param maxHops the maximum number of dependencies relations to follow during expansion.
  * @param maxLength the maximum allowed length of an entity in tokens.
  */
class RuleBasedEntityFinder(
  val entityEngine: ExtractorEngine,
  val avoidEngine: ExtractorEngine,
  val maxHops: Int,
  val maxLength: Int = RuleBasedEntityFinder.DEFAULT_MAX_LENGTH
) extends EntityFinder with LazyLogging {

  // avoid expanding along these dependencies
  val INVALID_OUTGOING = Set[scala.util.matching.Regex](
    "^prep_including$".r,
    "^prep_without$".r
  )

  val INVALID_INCOMING = Set[scala.util.matching.Regex](
    "^prep_with$".r,
    "^prep_without$".r,
    "^prep_except$".r,
    "^prep_despite$".r
  )

  // regexes describing valid outgoing dependencies
  val VALID_OUTGOING = Set[scala.util.matching.Regex](
    "^amod$".r, "^advmod$".r, "^ccmod$".r,
    "^dobj$".r,
    "^nn$".r,
    // ex.  "isotonic fluids may reduce the risk" -> "isotonic fluids may reduce the risk associated with X."
    "^vmod$".r,
    "^prep_".r
  )

  /**
    * Performs rule-based entity extraction with selective expansion along syntactic dependencies.
    * For filtering, see filterEntities.
    * @param doc a [[org.clulab.processors.Document]]
    */
  def extract(doc: Document): Seq[Mention] = {
    // avoid refs, etc.
    val avoid = avoidEngine.extractFrom(doc)
    val stateFromAvoid = State(avoid)
    val baseEntities = entityEngine.extractFrom(doc, stateFromAvoid).filter{ entity => ! stateFromAvoid.contains(entity) }
    val expandedEntities: Seq[Mention] = baseEntities.map(entity => expand(entity, maxHops))
    // split entities on likely coordinations
    val splitEntities = (baseEntities ++ expandedEntities).flatMap(splitCoordinatedEntities)
    // remove entity duplicates introduced by splitting expanded
    val distinctEntities = splitEntities.distinct
    // if there are no avoid mentions, no need to filter
    val res = if (avoid.isEmpty) {
      distinctEntities
    } else {
      // check that our expanded entities haven't swallowed any avoid mentions
      val avoidLabel = avoid.head.labels.last
      distinctEntities.filter{ m => stateFromAvoid.mentionsFor(m.sentence, m.tokenInterval, avoidLabel).isEmpty }
    }
    res
  }

  def extractAndFilter(doc: Document): Seq[Mention] = {
    val entities = extract(doc)
    filterEntities(entities)
  }

  /** Extracts entities without expanding or applying validation filters **/
  def extractBaseEntities(doc: Document): Seq[Mention] = {
    // avoid refs, etc.
    val avoid = avoidEngine.extractFrom(doc)
    val entities = entityEngine.extractFrom(doc, State(avoid))
    entities.filter(entity => ! avoid.contains(entity))
  }

  /**
    * Selects longest mentions among groups of overlapping entities
    * before applying a series of filtering constraints
    * Filter criteria: PoS tag validation of final token, bracket matching, and max length
    * @param entities entities to filter
    */
  private def filterEntities(entities: Seq[Mention]): Seq[Mention] = {
    // ignore citations and remove any entity that is too long given our criteria
    val filteredEntities = entities.filter(m => EntityConstraints.withinMaxLength(m, maxLength))
    val longest = RuleBasedEntityFinder.keepLongest(filteredEntities, new State())
    for {
      m <- longest
      if EntityConstraints.validFinalTag(m)
      if EntityConstraints.matchingBrackets(m)
    } yield m
  }

  /***
    * Recursively splits a TextBoundMention (flat) on likely coordinations.
    */
  @tailrec
  private def splitCoordinatedEntities(m: TextBoundMention, entities: Seq[Mention]): Seq[Mention] = {

    val coordIndex: Option[Int] = m.tokenInterval.find(i => isCoord(i, m))

    coordIndex match {
      // No coordination
      case None => entities ++ List(m)
      // mention contains only CC
      case Some(skipTok) if skipTok == m.start && m.end == m.start + 1 =>
        entities ++ List(m)
      // mention begins with CC, then skip this token and advance one
      case Some(skipTok) if skipTok == m.start =>
          val remaining = m.copy(tokenInterval = Interval(skipTok + 1, m.end))
          splitCoordinatedEntities(remaining, entities)
      // mention ends with CC, then discard and return
      case Some(skipTok) if skipTok == m.end - 1 =>
        val chunk = List(m.copy(tokenInterval = Interval(m.start, skipTok)))
        entities ++ chunk
      // otherwise, we need to split again
      case Some(idx) =>
        val chunk = if (m.start == idx) Nil else List(m.copy(tokenInterval = Interval(m.start, idx)))
        val remaining = m.copy(tokenInterval = Interval(idx + 1, m.end))
        splitCoordinatedEntities(remaining, entities ++ chunk)
    }
  }

  def splitCoordinatedEntities(m: Mention): Seq[Mention] = m match {
    case tb: TextBoundMention => splitCoordinatedEntities(tb, Nil)
    case _ => Seq(m)
  }

  /** Checks if the indexed token is a coordination **/
  def isCoord(i: Int, m: Mention): Boolean = EntityConstraints.isCoord(i, m)

  /**
    * Expands an entity up to the specified number of hops along valid grammatical relations.
    */
  def expand(entity: Mention, maxHops: Int): Mention = {
    val interval = traverseOutgoing(entity, maxHops)
    new TextBoundMention(entity.labels, interval, entity.sentence, entity.document, entity.keep, entity.foundBy)
  }

  /** Used by expand to selectively traverse the provided syntactic dependency graph **/
  @tailrec
  private def traverseOutgoing(
    tokens: Set[Int],
    newTokens: Set[Int],
    outgoingRelations: Array[Array[(Int, String)]],
    incomingRelations: Array[Array[(Int, String)]],
    remainingHops: Int
  ): Interval = {
    if (remainingHops == 0) {
      val allTokens = tokens ++ newTokens
      Interval(allTokens.min, allTokens.max + 1)
    } else {
      val newNewTokens = for {
        tok <- newTokens
        if outgoingRelations.nonEmpty && tok < outgoingRelations.length
        (nextTok, dep) <- outgoingRelations(tok)
        if isValidOutgoingDependency(dep)
        if hasValidIncomingDependencies(nextTok, incomingRelations)
      } yield nextTok
      traverseOutgoing(tokens ++ newTokens, newNewTokens, outgoingRelations, incomingRelations, remainingHops - 1)
    }
  }
  private def traverseOutgoing(m: Mention, numHops: Int): Interval = {
    val outgoing = outgoingEdges(m.sentenceObj)
    val incoming = incomingEdges(m.sentenceObj)
    traverseOutgoing(Set.empty, m.tokenInterval.toSet, outgoingRelations = outgoing, incomingRelations = incoming, numHops)
  }

  def outgoingEdges(s: Sentence): Array[Array[(Int, String)]] = s.dependencies match {
    case None => sys.error("sentence has no dependencies")
    case Some(dependencies) => dependencies.outgoingEdges
  }

  def incomingEdges(s: Sentence): Array[Array[(Int, String)]] = s.dependencies match {
    case None => sys.error("sentence has no dependencies")
    case Some(dependencies) => dependencies.incomingEdges
  }

  /** Ensure dependency may be safely traversed */
  def isValidOutgoingDependency(dep: String): Boolean = {
    VALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
      ! INVALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty)
  }

  /** Ensure current token does not have any incoming dependencies that are invalid **/
  def hasValidIncomingDependencies(tokenIdx: Int, incomingDependencies: Array[Array[(Int, String)]]): Boolean = {
    if (incomingDependencies.nonEmpty && tokenIdx < incomingDependencies.length) {
      incomingDependencies(tokenIdx).forall(pair => ! INVALID_INCOMING.exists(pattern => pattern.findFirstIn(pair._2).nonEmpty))
    } else true
  }

}


object RuleBasedEntityFinder extends LazyLogging {

  val DEFAULT_MAX_LENGTH = 10 // maximum length (in tokens) for an entity
  def apply(maxHops: Int, maxLength: Int = DEFAULT_MAX_LENGTH): RuleBasedEntityFinder = {
    val entityRules = ResourceUtils.readResource("org/clulab/openie/entities/grammar/entities.yml")
    val avoidRules = ResourceUtils.readResource("org/clulab/openie/entities/grammar/avoid.yml")

    val avoidEngine = ExtractorEngine(avoidRules)
    val entityEngine = ExtractorEngine(entityRules)
    new RuleBasedEntityFinder(avoidEngine = avoidEngine, entityEngine = entityEngine, maxHops = maxHops)
  }

  /** Keeps the longest mention for each group of overlapping mentions **/
  def keepLongest(mentions: Seq[Mention], state: State = new State()): Seq[Mention] = {
    val mns: Iterable[Mention] = for {
    // find mentions of the same label and sentence overlap
      (k, v) <- mentions.groupBy(m => (m.sentence, m.label))
      m <- v
      // for overlapping mentions starting at the same token, keep only the longest
      longest = v.filter(_.tokenInterval.overlaps(m.tokenInterval)).maxBy(m => m.end - m.start)
    } yield longest
    mns.toVector.distinct
  }

}