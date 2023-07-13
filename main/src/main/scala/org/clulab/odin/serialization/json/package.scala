package org.clulab.odin.serialization

import org.clulab.odin
import org.clulab.odin._
import org.clulab.struct.DirectedGraph
import org.clulab.utils.Hash
import org.json4s._
import org.json4s.JsonDSL._


package object json {

  import org.clulab.serialization.json.{JSONSerializer => _, _}

  private def argsAST(arguments: Map[String, Seq[Mention]]): JObject = {
    val args = arguments.map {
      case (name, mentions) => name -> JArray(mentions.map(_.jsonAST).toList)
    }
    JObject(args.toList)
  }

  /** Hash representing the [[Mention.arguments]] */
  // TODO: Compare this to Mention.argsHash().
  private def argsHash(arguments: Map[String, Seq[Mention]]): Int = {
    val argHashes = arguments.map { case (name, mentions) =>
      val seed = Hash(s"role:$name")
      val data = mentions.map(_.equivalenceHash)

      Hash.mix(seed, Hash.unordered(data))
    }
    // TODO: This is not the proper use of the count.
    Hash.withLast(Hash.unordered(argHashes))(
      Hash("org.clulab.odin.Mention.arguments")
    )
  }

  private def pathsAST(paths: Map[String, Map[Mention, odin.SynPath]]): JValue = paths match {
    case gps if gps.nonEmpty => gps.jsonAST
    case _ => JNothing
  }

  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

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

    def equivalenceHash: Int = Hash(
      Hash(stringCode),
      tb.labels.hashCode,
      tb.tokenInterval.start,
      tb.tokenInterval.end,
      tb.sentence,
      tb.document.equivalenceHash
    )

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

    def equivalenceHash: Int = Hash(
      Hash(stringCode),
      em.labels.hashCode,
      em.tokenInterval.start,
      em.tokenInterval.end,
      em.sentence,
      em.document.equivalenceHash,
      argsHash(em.arguments),
      TextBoundMentionOps(em.trigger).equivalenceHash
    )

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

    def equivalenceHash: Int = Hash(
      Hash(stringCode),
      rm.labels.hashCode,
      rm.tokenInterval.start,
      rm.tokenInterval.end,
      rm.sentence,
      rm.document.equivalenceHash,
      argsHash(rm.arguments)
    )

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

    def equivalenceHash: Int = Hash(
      Hash(stringCode),
      csm.labels.hashCode,
      csm.tokenInterval.start,
      csm.tokenInterval.end,
      csm.sentence,
      csm.document.equivalenceHash,
      argsHash(csm.arguments)
    )

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
      val simplePathMap: Map[String, Map[String, List[JValue]]] = paths.map { case (key, innermap) =>
        val pairs = for {
          (m: Mention, path: odin.SynPath) <- innermap.toList
          edgeAST = DirectedGraph.triplesToEdges[String](path.toList).map(_.jsonAST)
        } yield (m.id, edgeAST)
        key -> pairs.toMap
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
