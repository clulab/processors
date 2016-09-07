package org.clulab.serialization

import org.clulab.odin._
import org.clulab.processors.{Document, Sentence}
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.clulab.struct.{DirectedGraph, Edge, GraphMap}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native._
import scala.util.hashing.MurmurHash3._


package object json {

  trait JSONSerialization {

    def jsonAST: JValue

    def json(pretty: Boolean = false): String =
      if (pretty) prettyJson(renderJValue(jsonAST))
      else compactJson(renderJValue(jsonAST))

  }

  trait Equivalency {
    /** Custom hash used to establish equivalence */
    def equivalenceHash: Int

    /** string used to denote the class */
    val stringCode: String

    /** id encoding class info and equivalence */
    def id: String = s"$stringCode:$equivalenceHash"

  }

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
    }

    val stringCode: String = m match {
      case tb: TextBoundMention => TextBoundMentionOps(tb).stringCode
      case em: EventMention => EventMentionOps(em).stringCode
      case rm: RelationMention => RelationMentionOps(rm).stringCode
    }

    def equivalenceHash: Int = m match {
      case tb: TextBoundMention => TextBoundMentionOps(tb).equivalenceHash
      case em: EventMention => EventMentionOps(em).equivalenceHash
      case rm: RelationMention => RelationMentionOps(rm).equivalenceHash
    }

    override def id: String = m match {
      case tb: TextBoundMention => TextBoundMentionOps(tb).id
      case em: EventMention => EventMentionOps(em).id
      case rm: RelationMention => RelationMentionOps(rm).id
    }

    // A mention only only contains a pointer to a document, so
    // create a Seq[Mention] whose jsonAST includes
    // an accompanying json map of docEquivHash -> doc's json
    def completeAST: JValue = Seq(m).jsonAST

    /**
      * Serialize mentions to json file
      */
    def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), Seq(m).json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
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
      ("text" -> em.text) ~
      ("labels" -> em.labels) ~
      ("trigger" -> em.trigger.jsonAST) ~
      ("arguments" -> argsAST(em.arguments)) ~
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
      ("text" -> rm.text) ~
      ("labels" -> rm.labels) ~
      ("arguments" -> argsAST(rm.arguments)) ~
      ("tokenInterval" -> Map("start" -> rm.tokenInterval.start, "end" -> rm.tokenInterval.end)) ~
      ("characterStartOffset" -> rm.startOffset) ~
      ("characterEndOffset" -> rm.endOffset) ~
      ("sentence" -> rm.sentence) ~
      ("document" -> rm.document.equivalenceHash.toString) ~
      ("keep" -> rm.keep) ~
      ("foundBy" -> rm.foundBy)
    }
  }

  /** For sequences of mentions */
  implicit class MentionSeq(mentions: Seq[Mention]) extends JSONSerialization {

    def jsonAST: JValue = JSONSerializer.jsonAST(mentions)

    /**
      * Serialize mentions to json file
      */
    def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), mentions.json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }

  // Arrays cannot be directly converted to JValue
  implicit class ArrayOps(s: Option[Array[String]]) {
    def toSerializableJSON: Option[List[String]] = s match {
      case Some(s) => Some(s.toList)
      case None => None
    }
  }

  implicit class ODirectedGraphOps(odg: Option[DirectedGraph[String]]) {
    def toSerializableJSON: Option[JValue] = odg match {
      case Some(g) => Some(g.jsonAST)
      case None => None
    }
  }

  implicit class DirectedGraphOps(dg: DirectedGraph[String]) extends JSONSerialization {
    def jsonAST: JValue = {
      ("edges" -> dg.edges.map(_.jsonAST)) ~
      ("roots" -> dg.roots)
    }
  }

  implicit class EdgeOps(edge: Edge[String]) extends JSONSerialization {
    def jsonAST: JValue = {
      ("source" -> edge.source) ~
      ("destination" -> edge.destination) ~
      ("relation" -> edge.relation.toString)
    }
  }

  implicit class GraphMapOps(gm: GraphMap) extends JSONSerialization {
    def jsonAST: JValue = Extraction.decompose(gm.toMap.mapValues(_.jsonAST))
  }

  /** For Document */
  implicit class DocOps(doc: Document) extends JSONSerialization {

    def jsonAST: JValue = {
      // field and value are removed when value is not present
      ("id" -> doc.id) ~
      ("text" -> doc.text) ~
      ("sentences" -> doc.sentences.map(_.jsonAST).toList)
      // TODO: handle discourse tree
      //("discourse-tree" -> discourseTree)
    }

    /**
      * Serialize Document to json file
      */
    def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), doc.json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }


  /** For Sentence */
  implicit class SentenceOps(s: Sentence) extends JSONSerialization {

    def jsonAST: JValue = {
      ("words" -> s.words.toList) ~
      ("startOffsets" -> s.startOffsets.toList) ~
      ("endOffsets" -> s.endOffsets.toList) ~
      ("tags" -> s.tags.toSerializableJSON) ~
      ("lemmas" -> s.lemmas.toSerializableJSON) ~
      ("entities" -> s.entities.toSerializableJSON) ~
      ("norms" -> s.norms.toSerializableJSON) ~
      ("chunks" -> s.chunks.toSerializableJSON) ~
      ("graphs" -> s.dependenciesByType.jsonAST)
      // TODO: handle tree
      //("syntactic-tree") -> syntacticTree)
    }

    /**
      * Serialize Sentence to json file
      */
    def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), s.json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
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
}
