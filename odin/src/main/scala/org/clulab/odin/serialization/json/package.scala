package org.clulab.odin.serialization

import org.clulab.odin
import org.clulab.odin._
import org.clulab.struct.DirectedGraph
import org.json4s._
import org.json4s.JsonDSL._
import scala.util.hashing.MurmurHash3._


package object json {

  import org.clulab.serialization.json.{JSONSerializer => _, _}

  private def argsAST(arguments: Map[String, Seq[Mention]]): JObject = {
    val args = arguments.map {
      case (name, mentions) => name -> JArray(mentions.map(_.jsonAST).toList)
    }
    JObject(args.toList)
  }

  /** Hash representing the [[Mention.arguments]] */
  private def argsHash(args: Map[String, Seq[Mention]]): Int = {
    val argHashes = for {
      (role, mns) <- args
      bh = stringHash(s"role:$role")
      hs = mns.map(_.equivalenceHash)
    } yield mix(bh, unorderedHash(hs))
    val h0 = stringHash("org.clulab.odin.Mention.arguments")
    finalizeHash(h0, unorderedHash(argHashes))
  }

  private def pathsAST(paths: Map[String, Map[Mention, odin.SynPath]]): JValue = paths match {
    case gps if gps.nonEmpty => gps.jsonAST
    case _ => JNothing
  }

  implicit val formats = org.json4s.DefaultFormats

  implicit class MentionOps(m: Mention) extends JSONSerialization with Equivalency {

    def jsonAST: JValue = m match {
      case tb: TextBoundMention => TextBoundMentionOps(tb).jsonAST
      case em: EventMention => EventMentionOps(em).jsonAST
      case rm: RelationMention => RelationMentionOps(rm).jsonAST
      case csm: CrossSentenceMention => CrossSentenceMentionOps(csm).jsonAST
    }

    val stringCode: String = m match {
      case tb: TextBoundMention => TextBoundMentionOps(tb).stringCode
      case em: EventMention => EventMentionOps(em).stringCode
      case rm: RelationMention => RelationMentionOps(rm).stringCode
      case csm: CrossSentenceMention => CrossSentenceMentionOps(csm).stringCode
    }

    def equivalenceHash: Int = m match {
      case tb: TextBoundMention => TextBoundMentionOps(tb).equivalenceHash
      case em: EventMention => EventMentionOps(em).equivalenceHash
      case rm: RelationMention => RelationMentionOps(rm).equivalenceHash
      case csm: CrossSentenceMention => CrossSentenceMentionOps(csm).equivalenceHash
    }

    override def id: String = m match {
      case tb: TextBoundMention => TextBoundMentionOps(tb).id
      case em: EventMention => EventMentionOps(em).id
      case rm: RelationMention => RelationMentionOps(rm).id
      case csm: CrossSentenceMention => CrossSentenceMentionOps(csm).id
    }

    // A mention only only contains a pointer to a document, so
    // create a Seq[Mention] whose jsonAST includes
    // an accompanying json map of docEquivHash -> doc's json
    def completeAST: JValue = Seq(m).jsonAST

  }

  implicit class TextBoundMentionOps(tb: TextBoundMention) extends JSONSerialization with Equivalency {

    val stringCode = s"org.clulab.odin.${TextBoundMention.string}"

    def equivalenceHash: Int = {
      // the seed (not counted in the length of finalizeHash)
      val h0 = stringHash(stringCode)
      // labels
      val h1 = mix(h0, tb.labels.hashCode)
      // interval.start
      val h2 = mix(h1, tb.tokenInterval.start)
      // interval.end
      val h3 = mix(h2, tb.tokenInterval.end)
      // sentence index
      val h4 = mix(h3, tb.sentence)
      // document.equivalenceHash
      val h5 = mix(h4, tb.document.equivalenceHash)
      finalizeHash(h5, 5)
    }

    override def id: String = s"${TextBoundMention.shortString}:$equivalenceHash"

    def jsonAST: JValue = {
      ("type" -> TextBoundMention.string) ~
      // used for correspondence with paths map
      ("id" -> tb.id) ~
      ("text" -> tb.text) ~
      ("labels" -> tb.labels) ~
      ("tokenInterval" -> Map("start" -> tb.tokenInterval.start, "end" -> tb.tokenInterval.end)) ~
      ("characterStartOffset" -> tb.startOffset) ~
      ("characterEndOffset" -> tb.endOffset) ~
      ("sentence" -> tb.sentence) ~
      ("document" -> tb.document.equivalenceHash.toString) ~
      ("keep" -> tb.keep) ~
      ("foundBy" -> tb.foundBy)
    }
  }

  implicit class EventMentionOps(em: EventMention) extends JSONSerialization with Equivalency {

    val stringCode = s"org.clulab.odin.${EventMention.string}"

    def equivalenceHash: Int = {
      // the seed (not counted in the length of finalizeHash)
      val h0 = stringHash(stringCode)
      // labels
      val h1 = mix(h0, em.labels.hashCode)
      // interval.start
      val h2 = mix(h1, em.tokenInterval.start)
      // interval.end
      val h3 = mix(h2, em.tokenInterval.end)
      // sentence index
      val h4 = mix(h3, em.sentence)
      // document.equivalenceHash
      val h5 = mix(h4, em.document.equivalenceHash)
      // args
      val h6 = mix(h5, argsHash(em.arguments))
      // trigger
      val h7 = mix(h6, TextBoundMentionOps(em.trigger).equivalenceHash)
      finalizeHash(h7, 7)
    }

    override def id: String = s"${EventMention.shortString}:$equivalenceHash"

    def jsonAST: JValue = {
      ("type" -> EventMention.string) ~
      // used for paths map
      ("id" -> em.id) ~
      ("text" -> em.text) ~
      ("labels" -> em.labels) ~
      ("trigger" -> em.trigger.jsonAST) ~
      ("arguments" -> argsAST(em.arguments)) ~
      // paths are encoded as (arg name -> (mentionID -> path))
      ("paths" -> pathsAST(em.paths)) ~
      ("tokenInterval" -> Map("start" -> em.tokenInterval.start, "end" -> em.tokenInterval.end)) ~
      ("characterStartOffset" -> em.startOffset) ~
      ("characterEndOffset" -> em.endOffset) ~
      ("sentence" -> em.sentence) ~
      ("document" -> em.document.equivalenceHash.toString) ~
      ("keep" -> em.keep) ~
      ("foundBy" -> em.foundBy)
    }
  }

  implicit class RelationMentionOps(rm: RelationMention) extends JSONSerialization with Equivalency {

    val stringCode = s"org.clulab.odin.${RelationMention.string}"

    def equivalenceHash: Int = {
      // the seed (not counted in the length of finalizeHash)
      val h0 = stringHash(stringCode)
      // labels
      val h1 = mix(h0, rm.labels.hashCode)
      // interval.start
      val h2 = mix(h1, rm.tokenInterval.start)
      // interval.end
      val h3 = mix(h2, rm.tokenInterval.end)
      // sentence index
      val h4 = mix(h3, rm.sentence)
      // document.equivalenceHash
      val h5 = mix(h4, rm.document.equivalenceHash)
      // args
      val h6 = mix(h5, argsHash(rm.arguments))
      finalizeHash(h6, 6)
    }

    override def id: String = s"${RelationMention.shortString}:$equivalenceHash"

    def jsonAST: JValue = {
      ("type" -> RelationMention.string) ~
      // used for paths map
      ("id" -> rm.id) ~
      ("text" -> rm.text) ~
      ("labels" -> rm.labels) ~
      ("arguments" -> argsAST(rm.arguments)) ~
      // paths are encoded as (arg name -> (mentionID -> path))
      ("paths" -> pathsAST(rm.paths)) ~
      ("tokenInterval" -> Map("start" -> rm.tokenInterval.start, "end" -> rm.tokenInterval.end)) ~
      ("characterStartOffset" -> rm.startOffset) ~
      ("characterEndOffset" -> rm.endOffset) ~
      ("sentence" -> rm.sentence) ~
      ("document" -> rm.document.equivalenceHash.toString) ~
      ("keep" -> rm.keep) ~
      ("foundBy" -> rm.foundBy)
    }
  }

  implicit class CrossSentenceMentionOps(csm: CrossSentenceMention) extends JSONSerialization with Equivalency {

    val stringCode = s"org.clulab.odin.${CrossSentenceMention.string}"

    def equivalenceHash: Int = {
      // the seed (not counted in the length of finalizeHash)
      val h0 = stringHash(stringCode)
      // labels
      val h1 = mix(h0, csm.labels.hashCode)
      // interval.start
      val h2 = mix(h1, csm.tokenInterval.start)
      // interval.end
      val h3 = mix(h2, csm.tokenInterval.end)
      // sentence index
      val h4 = mix(h3, csm.sentence)
      // document.equivalenceHash
      val h5 = mix(h4, csm.document.equivalenceHash)
      // args
      val h6 = mix(h5, argsHash(csm.arguments))
      finalizeHash(h6, 6)
    }

    override def id: String = s"${CrossSentenceMention.shortString}:$equivalenceHash"

    def jsonAST: JValue = {
      ("type" -> CrossSentenceMention.string) ~
        // used for paths map
        ("id" -> csm.id) ~
        ("text" -> csm.text) ~
        ("labels" -> csm.labels) ~
        ("anchor" -> csm.anchor.id) ~
        ("neighbor" -> csm.anchor.id) ~
        ("arguments" -> argsAST(csm.arguments)) ~
        ("tokenInterval" -> Map("start" -> csm.tokenInterval.start, "end" -> csm.tokenInterval.end)) ~
        ("sentence" -> csm.sentence) ~
        ("document" -> csm.document.equivalenceHash.toString) ~
        ("keep" -> csm.keep) ~
        ("foundBy" -> csm.foundBy)
    }
  }

  /** For sequences of mentions */
  implicit class MentionSeq(mentions: Seq[Mention]) extends JSONSerialization {

    def jsonAST: JValue = JSONSerializer.jsonAST(mentions)

  }

  // Syntactic paths generalized are graph paths
  implicit class OdinPathOps(paths: Map[String, Map[Mention, odin.SynPath]]) extends JSONSerialization {
    // simplify paths by ignoring Mentions
    def jsonAST: JValue = {
      val simplePathMap: Map[String, Map[String, List[JValue]]] = paths.mapValues{ innermap =>
        val pairs = for {
          (m: Mention, path: odin.SynPath) <- innermap.toList
          edgeAST = DirectedGraph.triplesToEdges[String](path.toList).map(_.jsonAST)
        } yield (m.id, edgeAST)
        pairs.toMap
      }
      simplePathMap
    }
  }

  object TextBoundMention {
    val string = "TextBoundMention"
    val shortString = "T"
  }

  object EventMention {
    val string = "EventMention"
    val shortString = "E"
  }

  object RelationMention {
    val string = "RelationMention"
    val shortString = "R"
  }

  object CrossSentenceMention {
    val string = "CrossSentenceMention"
    val shortString = "CS"
  }
}
