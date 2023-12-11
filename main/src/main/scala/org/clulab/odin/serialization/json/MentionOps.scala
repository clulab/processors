package org.clulab.odin.serialization.json

import org.clulab.odin
import org.clulab.odin._
import org.clulab.serialization.json.{JSONSerializer => _, _}
import org.clulab.struct.DirectedGraph
import org.clulab.utils.Hash
import org.clulab.utils.Unordered
import org.clulab.utils.Unordered.OrderingOrElseBy
import org.json4s.{DefaultFormats, JArray, JNothing, JObject, JValue}
import org.json4s.JsonDSL._

import scala.math.Ordering.Implicits._ // Allow Seqs to be compared to each other.
import scala.util.hashing.MurmurHash3._

object MentionOps {
  // As Scala 3 points out, this can be recursive, because the arguments contain more Mentions.
  // In part because of this, only the argument keys are used in sort, not the Mention values.
  // Triggers are not taken into account and neither is the class of he Mention except through the name.
  implicit val mentionOrdering: Ordering[Mention] = Unordered[Mention]
      .orElseBy(_.sentence)
      .orElseBy(_.tokenInterval)
      .orElseBy(_.labels)
      .orElseBy(_.foundBy)
      .orElseBy(_.getClass.getName)
      .orElseBy(_.arguments.keys.toSeq.sorted)
      // The above gets all but a very few.
      .orElseBy(_.arguments.values.flatten.map(_.tokenInterval).toSeq.sorted)
      // Skip paths.

  // Scala 2.11 requires this for some reason.
  implicit val pathsOrdering: Ordering[(Mention, List[(Int, Int, String)])] = Unordered[(Mention, List[(Int, Int, String)])]
      .orElseBy(_._1)
      .orElseBy(_._2)

  // Lists are produced so that they can be quickly converted to JObjects and JArrays.
  // Maps cannot be used because the order of the keys is not fixed.
  def flattenArguments(mention: Mention): List[(String, List[Mention])] = {
    // First sort the keys and then sort the corresponding values.
    val sortedKeys = mention.arguments.keys.toList.sorted

    sortedKeys.map { key =>
      // For efficience, sort smaller lists of these per key values rather than
      // one large List[(key, value)].
      val sortedValues = mention.arguments(key).sorted.toList

      key -> sortedValues
    }
  }

  // Lists are produced so that they can be quickly converted to JObjects.
  // Maps cannot be used because the order of the keys is not fixed.
  def flattenPaths(mention: Mention): List[(String, List[(Mention, List[(Int, Int, String)])])] = {
    // Again, first sort the keys and then sort the corresponding values.
    val sortedKeys = mention.paths.keys.toList.sorted

    sortedKeys.map { key =>
      val unsortedValues = mention.paths(key).toList.map { case (mention, synPath) =>
        // These are being left as tuples, because it seems quite expensive to sort
        // the keys and then track down their values.  There are other ways, though.
        val synPathList: List[(Int, Int, String)] = synPath.toList

        (mention, synPathList)
      }
      val sortedValues = unsortedValues.sorted // This is handled by pathsOrdering.

      key -> sortedValues
    }
  }

  def apply(mention: Mention): MentionOps = {
    mention match {
      case mention: TextBoundMention => new TextBoundMentionOps(mention)
      case mention: EventMention => new EventMentionOps(mention)
      case mention: RelationMention => new RelationMentionOps(mention)
      case mention: CrossSentenceMention => new CrossSentenceMentionOps(mention)
    }
  }
}

// The Mention is now recorded in a val so that mixed-in traits can make use of it.
abstract class MentionOps(val mention: Mention) extends JSONSerialization with Equivalency {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  // This is a very expensive calculation.  Do not perform it more than once.
  // The Document is assumed not to change during the lifetime of this object.
  lazy val documentEquivalenceHash: Int = mention.document.equivalenceHash
  // This could be mention.getClass.getName, but we're apparently guarding against
  // refactorizations that could change the value and against automatically using
  // the value from subclasses.  They should explicitly change their stringCode.
  val namespace = "org.clulab.odin"
  val stringCode = s"$namespace.$longString"

  def longString: String
  def shortString: String

  def jsonAST: JValue
  def equivalenceHash: Int

  override def id: String = s"$shortString:$equivalenceHash"

  def asMentionOps(mention: Mention): MentionOps = MentionOps(mention)

  protected def argsAST: JObject = {
    val flattenedArguments = MentionOps.flattenArguments(mention)
    val args = flattenedArguments.map { case (name, mentions) =>
      name -> JArray(mentions.map(asMentionOps(_).jsonAST))
    }

    JObject(args)
  }

  /** Hash representing the [[Mention.arguments]] */
  // TODO: Compare this to Mention.argsHash().
  protected def argsHash(arguments: Map[String, Seq[Mention]]): Int = {
    val argHashes = arguments.map { case (name, mentions) =>
      val seed = Hash(s"role:$name")
      val data = mentions.map(asMentionOps(_).equivalenceHash)

      Hash.mix(seed, Hash.unordered(data))
    }
    // TODO: This is not the proper use of the count.
    Hash.withLast(Hash.unordered(argHashes))(
      Hash("org.clulab.odin.Mention.arguments")
    )
  }

  protected def pathsAST: JValue = {
    val argumentMentionIds = mention.arguments.values.flatten.map(MentionOps(_).id).toSet
    // Would removing a path mess up the sort order?  We are not sorting on paths.
    val flattenedPaths = MentionOps.flattenPaths(mention)
    // Convert all the mentions to their IDs just once, now.
    // Confirm externally to this that all IDs are unique.
    // mapValues is not available without warning in Scala 2.13+.
    val mentionIdPaths: List[(String, List[(String, List[(Int, Int, String)])])] = flattenedPaths.map { case (key, innerMap) =>
      val newInnerMap = innerMap.map { case (mention, synPath) =>
        MentionOps(mention).id -> synPath
      }

      key -> newInnerMap
    }
    // Only use mentions whose IDs are in argumentMentionIds.
    val argumentPaths = mentionIdPaths.map { case (key, innerMap) =>
      val newInnerMap = innerMap.filter { case (mentionId, _) => argumentMentionIds(mentionId) }

      key -> newInnerMap
    }
    // Only keep the paths for which the innerMap is nonEmpty.
    val nonEmptyArgumentPaths = argumentPaths.filter { case (_, innerMap) =>
      innerMap.nonEmpty
    }

    if (nonEmptyArgumentPaths.isEmpty) JNothing
    else {
      // Do not use Maps here because the order of the keys is not fixed.
      val outerPairs = nonEmptyArgumentPaths.map { case (key, innermap) =>
        val innerPairs = innermap.map { case (mentionId, synPath) =>
          val edgeAST = DirectedGraph.triplesToEdges[String](synPath).map(_.jsonAST)
        
          mentionId -> JArray(edgeAST)
        }

        key -> JObject(innerPairs)
      }

      val result = JObject(outerPairs)
      result
    }
  }
}

/** Calls to jsonAST will result in "id" and "document" values being calculated.  The id comes from the
  * id method which calls equivalenceHash which in turn calls documentEquivalenceHash.  The document
  * comes directly from documentEquivalenceHash.  The hash is an expensive calculation, so it is stored
  * here in a lazy val rather than a def to prevent recalculation with the assumption that the value
  * will not change during the serialization of the Mention.  Subclasses that know the Document does
  * not change across serialization of multiple Mentions may want to implement more extensive caching.
  */
class TextBoundMentionOps(tb: TextBoundMention) extends MentionOps(tb) {

  def longString: String = TextBoundMentionOps.string

  def shortString: String = TextBoundMentionOps.shortString

  override def equivalenceHash: Int = Hash(
    Hash(stringCode),
    tb.labels.hashCode,
    tb.tokenInterval.start,
    tb.tokenInterval.end,
    tb.sentence,
    documentEquivalenceHash
  )

  override def jsonAST: JValue = {
    ("type" -> longString) ~
    // used for correspondence with paths map
    ("id" -> id) ~ // tb.id would just create a different TextBoundMentionOps to provide the id
    ("text" -> tb.text) ~
    ("labels" -> tb.labels) ~
    ("tokenInterval" -> Map("start" -> tb.tokenInterval.start, "end" -> tb.tokenInterval.end)) ~
    ("characterStartOffset" -> tb.startOffset) ~
    ("characterEndOffset" -> tb.endOffset) ~
    ("sentence" -> tb.sentence) ~
    ("document" -> documentEquivalenceHash.toString) ~
    ("keep" -> tb.keep) ~
    ("foundBy" -> tb.foundBy)
  }
}

/** Child Mentions, in this case the trigger and arguments, may not be the exact type
  * expected, especially if EventMention has been subclassed.  Therefore, the jsonAST
  * conversions have been isolated in a method that can be overridden by subclasses.
  */
class EventMentionOps(em: EventMention) extends MentionOps(em) {

  def longString: String = EventMentionOps.string

  def shortString: String = EventMentionOps.shortString

  def triggerAST: JValue = asMentionOps(em.trigger).jsonAST

  override def equivalenceHash: Int = Hash(
    Hash(stringCode),
    em.labels.hashCode,
    em.tokenInterval.start,
    em.tokenInterval.end,
    em.sentence,
    documentEquivalenceHash,
    argsHash(em.arguments),
    asMentionOps(em.trigger).equivalenceHash
  )

  override def jsonAST: JValue = {
    ("type" -> longString) ~
    // used for paths map
    ("id" -> id) ~ // em.id would just create a different EventMentionOps to provide the id
    ("text" -> em.text) ~
    ("labels" -> em.labels) ~
    ("trigger" -> triggerAST) ~
    ("arguments" -> argsAST) ~
    // paths are encoded as (arg name -> (mentionID -> path))
    ("paths" -> pathsAST) ~
    ("tokenInterval" -> Map("start" -> em.tokenInterval.start, "end" -> em.tokenInterval.end)) ~
    ("characterStartOffset" -> em.startOffset) ~
    ("characterEndOffset" -> em.endOffset) ~
    ("sentence" -> em.sentence) ~
    ("document" -> documentEquivalenceHash.toString) ~
    ("keep" -> em.keep) ~
    ("foundBy" -> em.foundBy)
  }
}

class RelationMentionOps(rm: RelationMention) extends MentionOps(rm) {

  def longString: String = RelationMentionOps.string

  def shortString: String = RelationMentionOps.shortString

  override def equivalenceHash: Int = Hash(
    Hash(stringCode),
    rm.labels.hashCode,
    rm.tokenInterval.start,
    rm.tokenInterval.end,
    rm.sentence,
    documentEquivalenceHash,
    argsHash(rm.arguments)
  )

  override def jsonAST: JValue = {
    ("type" -> longString) ~
    // used for paths map
    ("id" -> id) ~ // rm.id would just create a different RelationMentionOps to provide the id
    ("text" -> rm.text) ~
    ("labels" -> rm.labels) ~
    ("arguments" -> argsAST) ~
    // paths are encoded as (arg name -> (mentionID -> path))
    ("paths" -> pathsAST) ~
    ("tokenInterval" -> Map("start" -> rm.tokenInterval.start, "end" -> rm.tokenInterval.end)) ~
    ("characterStartOffset" -> rm.startOffset) ~
    ("characterEndOffset" -> rm.endOffset) ~
    ("sentence" -> rm.sentence) ~
    ("document" -> documentEquivalenceHash.toString) ~
    ("keep" -> rm.keep) ~
    ("foundBy" -> rm.foundBy)
  }
}

class CrossSentenceMentionOps(csm: CrossSentenceMention) extends MentionOps(csm) {

  def longString: String = CrossSentenceMentionOps.string

  def shortString: String = CrossSentenceMentionOps.shortString

  override def equivalenceHash: Int = Hash(
    Hash(stringCode),
    csm.labels.hashCode,
    csm.tokenInterval.start,
    csm.tokenInterval.end,
    csm.sentence,
    documentEquivalenceHash,
    argsHash(csm.arguments)
  )

  override def jsonAST: JValue = {
    ("type" -> longString) ~
    // used for paths map
    ("id" -> id) ~ // csm.id would just create a different CrossSentenceMentionOps to provide the id
    ("text" -> csm.text) ~
    ("labels" -> csm.labels) ~
    ("anchor" -> asMentionOps(csm.anchor).id) ~
    ("neighbor" -> asMentionOps(csm.anchor).id) ~
    ("arguments" -> argsAST) ~
    ("tokenInterval" -> Map("start" -> csm.tokenInterval.start, "end" -> csm.tokenInterval.end)) ~
    ("sentence" -> csm.sentence) ~
    ("document" -> documentEquivalenceHash.toString) ~
    ("keep" -> csm.keep) ~
    ("foundBy" -> csm.foundBy)
  }
}

object TextBoundMentionOps {
  val string = "TextBoundMention"
  val shortString = "T"
}

object EventMentionOps {
  val string = "EventMention"
  val shortString = "E"
}

object RelationMentionOps {
  val string = "RelationMention"
  val shortString = "R"
}

object CrossSentenceMentionOps {
  val string = "CrossSentenceMention"
  val shortString = "CS"
}
