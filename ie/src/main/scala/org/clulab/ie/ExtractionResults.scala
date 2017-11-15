package org.clulab.ie

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.apache.commons.io.IOUtils
import org.clulab.odin.Mention
//import org.clulab.serialization.json.{ JSONSerialization }//, MentionOps, MentionSeq }
import org.clulab.serialization.json.JSONSerialization
import org.clulab.odin.serialization.json.{ MentionOps, MentionSeq, JSONSerializer }
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._


case class ExtractionResults(results: Vector[(Mention, MentionMetaData)]) extends JSONSerialization {

  override def jsonAST: JValue = {

    implicit val formats = DefaultFormats

    val mentionsJSON: JValue = results.map(_._1).jsonAST
    val metadataJAST: JValue = "metadata" -> results.map(mm => Extraction.decompose(mm._2)).toList
    mentionsJSON merge metadataJAST
  }

//  // FIXME: remove these after upgrading processors
//  /**
//    * Serialize mentions to json file
//    */
//  def saveJSON(file: String, pretty: Boolean): Unit = {
//    require(file.endsWith(".json"), "file should have .json extension")
//    val _ = Files.write(Paths.get(file), this.json(pretty).getBytes(StandardCharsets.UTF_8))
//  }
//  def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)

}

object ExtractionResults {

  implicit val formats = DefaultFormats

  def fromString(s: String): ExtractionResults = fromJValue(parse(s, useBigDecimalForDouble = false))

  def fromStream(is: InputStream): ExtractionResults = {
    fromString(IOUtils.toString(is, StandardCharsets.UTF_8))
  }

  def fromFile(f: File): ExtractionResults = {
    val source = scala.io.Source.fromFile(f)
    val contents = source.getLines.mkString
    source.close()
    fromString(contents)
  }

  def fromJValue(json: JValue): ExtractionResults = {
    val mentions = JSONSerializer.toMentions(json)
    val metadataLUT = (json \ "metadata").extract[List[MentionMetaData]].groupBy(_.mentionID)
    val res = mentions.map( m => (m, metadataLUT(m.id).head)).toVector
    ExtractionResults(res)
  }

}
