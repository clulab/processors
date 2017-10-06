package org.clulab.odin.serialization.json

import java.io.File

import scala.io.Source

import org.clulab.processors.Document
import org.clulab.struct.{DirectedGraph, Edge, Interval}
import org.clulab.odin
import org.clulab.odin._
import org.clulab.serialization.json.DocOps

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._


/** JSON serialization utilities */
object JSONSerializer {

  import org.clulab.odin.serialization.json._
  import org.clulab.serialization.json.JSONSerializer.toDocument

  def jsonAST(mentions: Seq[Mention]): JValue = {
    val docsMap: Map[String, JValue] = {
      // create a set of Documents
      // in order to avoid calling jsonAST for duplicate docs
      val docs: Set[Document] = mentions.map(m => m.document).toSet
      docs.map(doc => doc.equivalenceHash.toString -> doc.jsonAST)
        .toMap
    }
    val mentionList = JArray(mentions.map(_.jsonAST).toList)

    ("documents" -> docsMap) ~
    ("mentions" -> mentionList)
  }

  def jsonAST(f: File): JValue = {
    val source = Source.fromFile(f)
    val contents = source.getLines.mkString
    source.close()
    parse(contents)
  }

  /** Produce a sequence of mentions from json */
  def toMentions(json: JValue): Seq[Mention] = {

    require(json \ "documents" != JNothing, "\"documents\" key missing from json")
    require(json \ "mentions" != JNothing, "\"mentions\" key missing from json")

    // build the documents once
    val docMap = mkDocumentMap(json \ "documents")
    val mmjson = (json \ "mentions").asInstanceOf[JArray]

    mmjson.arr.map(mjson => toMention(mjson, docMap))
  }
  /** Produce a sequence of mentions from a json file */
  def toMentions(file: File): Seq[Mention] = toMentions(jsonAST(file))

  /** Build mention from json of mention and corresponding json map of documents <br>
    * Since a single Document can be quite large and may be shared by multiple mentions,
    * only a reference to the document json is contained within each mention.
    * A map from doc reference to document json is used to avoid redundancies and reduce file size during serialization.
    * */
  def toMention(mjson: JValue, docMap: Map[String, Document]): Mention = {

    val tokInterval = Interval(
      (mjson \ "tokenInterval" \ "start").extract[Int],
      (mjson \ "tokenInterval" \ "end").extract[Int]
    )
    // elements shared by all Mention types
    val labels = (mjson \ "labels").extract[List[String]]
    val sentence = (mjson \ "sentence").extract[Int]
    val docHash = (mjson \ "document").extract[String]
    val document = docMap(docHash)
    val keep = (mjson \ "keep").extract[Boolean]
    val foundBy = (mjson \ "foundBy").extract[String]

    def mkArgumentsFromJsonAST(json: JValue): Map[String, Seq[Mention]] = try {
      val args = json.extract[Map[String, JArray]]
      val argPairs = for {
        (k: String, v: JValue) <- args
        mns: Seq[Mention] = v.arr.map(m => toMention(m, docMap))
      } yield (k, mns)
      argPairs
    } catch {
      case e: org.json4s.MappingException => Map.empty[String, Seq[Mention]]
    }



    /** Build mention paths from json */
    def toPaths(json: JValue, docMap: Map[String, Document]): Map[String, Map[Mention, odin.SynPath]] = {

      /** Create mention from args json for given id */
      def findMention(mentionID: String, json: JValue, docMap: Map[String, Document]): Option[Mention] = {
        // inspect arguments for matching ID
        json \ "arguments" match {
          // if we don't have arguments, we can't produce a Mention
          case JNothing => None
          // Ahoy! There be args!
          case something =>
            // flatten the Seq[Mention.jsonAST] for each arg
            val argsjson: Iterable[JValue] = for {
              mnsjson: JArray <- something.extract[Map[String, JArray]].values
              mjson <- mnsjson.arr
              if (mjson \ "id").extract[String] == mentionID
            } yield mjson

            argsjson.toList match {
              case Nil => None
              case j :: _ => Some(toMention(j, docMap))
            }
        }
      }

      // build paths
      json \ "paths" match {
        case JNothing => Map.empty[String, Map[Mention, odin.SynPath]]
        case contents => for {
          (role, innermap) <- contents.extract[Map[String, Map[String, JValue]]]
        } yield {
          // make inner map (Map[Mention, odin.SynPath])
          val pathMap = for {
            (mentionID: String, pathJSON: JValue) <- innermap.toSeq
            mOp = findMention(mentionID, json, docMap)
            // were we able to recover a mention?
            if mOp.nonEmpty
            m = mOp.get
            edges: Seq[Edge[String]] = pathJSON.extract[Seq[Edge[String]]]
            synPath: odin.SynPath = DirectedGraph.edgesToTriples[String](edges)
          } yield m -> synPath
          // marry role with (arg -> path) info
          role -> pathMap.toMap
        }
      }
    }

    // build Mention
    mjson \ "type" match {
      case JString(EventMention.string) =>
        new EventMention(
          labels,
          tokInterval,
          // trigger must be TextBoundMention
          toMention(mjson \ "trigger", docMap).asInstanceOf[TextBoundMention],
          mkArgumentsFromJsonAST(mjson \ "arguments"),
          paths = toPaths(mjson, docMap),
          sentence,
          document,
          keep,
          foundBy
        )
      case JString(RelationMention.string) =>
        new RelationMention(
          labels,
          tokInterval,
          mkArgumentsFromJsonAST(mjson \ "arguments"),
          paths = toPaths(mjson, docMap),
          sentence,
          document,
          keep,
          foundBy
        )
      case JString(TextBoundMention.string) =>
        new TextBoundMention(
          labels,
          tokInterval,
          sentence,
          document,
          keep,
          foundBy
        )
      case JString(CrossSentenceMention.string) =>
        val args = mkArgumentsFromJsonAST(mjson \ "arguments")
        val anchorID = (mjson \ "anchor").extract[String]
        val neighborID = (mjson \ "neighbor").extract[String]
        //toMention(mjson \ "neighbor", docMap)
        new CrossSentenceMention(
          labels,
          anchor = args.values.flatten.find(_.id == anchorID).get,
          neighbor = args.values.flatten.find(_.id == neighborID).get,
          mkArgumentsFromJsonAST(mjson \ "arguments"),
          document,
          keep,
          foundBy
        )

      case other => throw new Exception(s"unrecognized mention type '${other.toString}'")
    }
  }

  /** create a map pointing from a Doc.equivalenceHash -> Document */
  def mkDocumentMap(djson: JValue): Map[String, Document] = {
    val kvPairs: List[(String, Document)] = for {
      JObject(kvpair) <- djson
      JField(docHash: String, docjson) <- kvpair
      // this child should contain sentences
      if (docjson \ "sentences") != JNothing
    } yield docHash -> toDocument(docjson)

    kvPairs.toMap
  }

  private def getStringOption(json: JValue, key: String): Option[String] = json \ key match {
    case JString(s) => Some(s)
    case _ => None
  }
}
