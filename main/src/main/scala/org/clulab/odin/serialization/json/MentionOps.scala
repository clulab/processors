package org.clulab.odin.serialization.json

import org.clulab.odin
import org.clulab.odin._
import org.clulab.serialization.json.{JSONSerializer => _, _}
import org.clulab.struct.DirectedGraph
import org.json4s._
import org.json4s.JsonDSL._

import scala.util.hashing.MurmurHash3._

object MentionOps {

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
    val args = mention.arguments.map {
      case (name, mentions) => name -> JArray(mentions.map(asMentionOps(_).jsonAST).toList)
    }
    JObject(args.toList)
  }

  /** Hash representing the [[Mention.arguments]] */
  protected def argsHash: Int = {
    val argHashes = for {
      (role, mns) <- mention.arguments
      bh = stringHash(s"role:$role")
      hs = mns.map(asMentionOps(_).equivalenceHash)
    } yield mix(bh, unorderedHash(hs))
    val h0 = stringHash("org.clulab.odin.Mention.arguments")
    finalizeHash(h0, unorderedHash(argHashes))
  }

  protected def pathsAST: JValue = {
    if (mention.paths.isEmpty) JNothing
    else {
      val simplePathMap: Map[String, Map[String, List[JValue]]] = mention.paths.map { case (key, innermap) =>
        val pairs = for {
          (m: Mention, path: odin.SynPath) <- innermap.toList
          edgeAST = DirectedGraph.triplesToEdges[String](path.toList).map(_.jsonAST)
        } yield (asMentionOps(m).id, edgeAST)
        key -> pairs.toMap
      }
      simplePathMap
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

  override def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // The stringHash is based on information from this class, not the actual class.
    val h0 = stringHash(s"$namespace.${TextBoundMentionOps.string}") // (stringCode)
    // labels
    val h1 = mix(h0, tb.labels.hashCode)
    // interval.start
    val h2 = mix(h1, tb.tokenInterval.start)
    // interval.end
    val h3 = mix(h2, tb.tokenInterval.end)
    // sentence index
    val h4 = mix(h3, tb.sentence)
    // document.equivalenceHash
    val h5 = mix(h4, documentEquivalenceHash)
    finalizeHash(h5, 5)
  }

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

  override def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // The stringHash is based on information from this class, not the actual class.
    val h0 = stringHash(s"$namespace.${EventMentionOps.string}") // (stringCode)
    // labels
    val h1 = mix(h0, em.labels.hashCode)
    // interval.start
    val h2 = mix(h1, em.tokenInterval.start)
    // interval.end
    val h3 = mix(h2, em.tokenInterval.end)
    // sentence index
    val h4 = mix(h3, em.sentence)
    // document.equivalenceHash
    val h5 = mix(h4, documentEquivalenceHash)
    // args
    val h6 = mix(h5, argsHash)
    // trigger
    val h7 = mix(h6, asMentionOps(em.trigger).equivalenceHash)
    finalizeHash(h7, 7)
  }

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

  override def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // The stringHash is based on information from this class, not the actual class.
    val h0 = stringHash(s"$namespace.${RelationMentionOps.string}") // (stringCode)
    // labels
    val h1 = mix(h0, rm.labels.hashCode)
    // interval.start
    val h2 = mix(h1, rm.tokenInterval.start)
    // interval.end
    val h3 = mix(h2, rm.tokenInterval.end)
    // sentence index
    val h4 = mix(h3, rm.sentence)
    // document.equivalenceHash
    val h5 = mix(h4, documentEquivalenceHash)
    // args
    val h6 = mix(h5, argsHash)
    finalizeHash(h6, 6)
  }

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

  override def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // The stringHash is based on information from this class, not the actual class.
    val h0 = stringHash(s"$namespace.${CrossSentenceMentionOps.string}") // (stringCode)
    // labels
    val h1 = mix(h0, csm.labels.hashCode)
    // interval.start
    val h2 = mix(h1, csm.tokenInterval.start)
    // interval.end
    val h3 = mix(h2, csm.tokenInterval.end)
    // sentence index
    val h4 = mix(h3, csm.sentence)
    // document.equivalenceHash
    val h5 = mix(h4, documentEquivalenceHash)
    // args
    val h6 = mix(h5, argsHash)
    finalizeHash(h6, 6)
  }

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
