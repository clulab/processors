package org.clulab.ie

import org.clulab.odin.{ Mention, TextBoundMention }
import org.clulab.processors.{ Document, Sentence }
import org.clulab.struct.Interval
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write


trait CharacterSpan {
  val start: Int
  val end: Int
}

case class NamedCharacterSpan(label: String, attributes: Map[String, String], start: Int, end: Int) extends CharacterSpan

object NamedCharacterSpan {

  implicit val jsonDefaultFormats = DefaultFormats

  def spansToJson(ncss: Seq[NamedCharacterSpan]): String = write(ncss)

  def jsonToSpans(json: String): Seq[NamedCharacterSpan] = parse(json, useBigDecimalForDouble = false).extract[Seq[NamedCharacterSpan]]

  def toMention(ncs: NamedCharacterSpan, doc: Document): Mention = {

    def findSpan(s: Sentence, ncs: NamedCharacterSpan): Option[(Int, Int)] = {
      val start = ncs.start
      val end = ncs.end
      val startIdx: Option[Int] = s.words.indices.find{ i =>
        // offsets for a single token
        val startCharOffset: Int = s.startOffsets(i)
        val endCharOffset: Int = s.endOffsets(i)
        start >= startCharOffset && start <= endCharOffset
      }
      val endIdx: Option[Int] = s.words.indices.find{ i =>
        // offsets for a single token
        val startCharOffset: Int = s.startOffsets(i)
        val endCharOffset: Int = s.endOffsets(i)
        end >= startCharOffset && end <= endCharOffset
      }
      if (startIdx.isDefined && endIdx.isDefined) Some((startIdx.get, endIdx.get)) else None
    }

    val res: Seq[Mention] = for {
      (s, i) <- doc.sentences.zipWithIndex
      span = findSpan(s, ncs)
      if span.nonEmpty
    } yield {
      new TextBoundMention(
        label = ncs.label,
        tokenInterval = Interval(start = span.get._1, end = span.get._2 + 1),
        sentence = i,
        document = doc,
        keep = true,
        foundBy = s"named-character-span-${ncs.label}"
      )
    }
    assert(res.size == 1)
    res.head
  }

}