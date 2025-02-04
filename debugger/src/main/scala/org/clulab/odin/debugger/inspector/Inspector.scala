package org.clulab.odin.debugger.inspector

import org.clulab.odin.debugger.DebuggerRecord
import org.clulab.odin.impl.{Extractor, Inst}
import org.clulab.processors.Sentence
import scalatags.Text.all._

import scala.collection.mutable

class EqualityByIdentity(val any: Any) {

  override def hashCode(): Int = any.hashCode()

  override def equals(other: Any): Boolean =
    if (other.isInstanceOf[EqualityByIdentity])
      (this.any, other.asInstanceOf[EqualityByIdentity].any) match {
        case (self: Byte, other: Byte) => self == other
        case (self: Short, other: Short) => self == other
        case (self: Int, other: Int) => self == other
        case (self: Long, other: Long) => self == other
        case (self: Float, other: Float) => self == other
        case (self: Double, other: Double) => self == other
        case (self: AnyRef, other: AnyRef) =>
          val result = self.eq(other)
          result
        case _ => false
      }
    else false
}

object EqualityByIdentity {

  def apply(any: Any): EqualityByIdentity = new EqualityByIdentity(any)
}


class Inspector(transcript: mutable.Buffer[DebuggerRecord]) {

  def inspectExtractor(extractor: Extractor): Inspector = {
    val newTranscript = transcript.filter { debuggerRecord =>
      debuggerRecord.extractor.eq(extractor)
    }

    new Inspector(newTranscript)
  }

  def inspectSentence(sentence: Sentence): Inspector = {
    val newTranscript = transcript.filter { debuggerRecord =>
      debuggerRecord.sentence.eq(sentence)
    }

    new Inspector(newTranscript)
  }

  def mkHtmlTable(sentence: Sentence): String = {
    val sentenceTranscript = transcript.filter { debuggerRecord =>
      debuggerRecord.sentence.eq(sentence)
    }
    val headers = "start" +: sentence.words

    def findMatches(start: Int, tok: Int): Seq[(Inst, Option[Boolean])] = {
      val matchTranscripts = sentenceTranscript.filter { debuggerRecord =>
        debuggerRecord.startOpt.contains(start) && debuggerRecord.tokOpt.contains(tok)
      }
      val trues = matchTranscripts.filter(_.matches).flatMap(_.instOpt).distinct.toSet
      val falses = matchTranscripts.filter(!_.matches).flatMap(_.instOpt).distinct.toSet
      val unsortedAll = (trues ++ falses).toSeq.sortBy(_.getPosId)
      val sortedAll =
          if (unsortedAll.nonEmpty && unsortedAll.head.getPosId == 0)
            unsortedAll.tail :+ unsortedAll.head
          else
            unsortedAll
      val summary = sortedAll.map { inst =>
        ((trues.contains(inst), falses.contains(inst))) match {
          case (true, true) => None
          case (true, false) => Some(true)
          case (false, true) => Some(false)
          case (false, false) => None
        }
      }
      sortedAll.zip(summary)
    }
    val borderStyle = "border: 1px solid black; border-collapse: collapse"
    val fragment = html(body(
      table(style := borderStyle)(
        tr(
          th(style := borderStyle)("start"),
          sentence.words.map { word =>
            th(style := borderStyle)(word)
          }
        ),
        sentence.words.indices.map { start =>
          tr(
            td(style := borderStyle)(sentence.words(start)),
            sentence.words.indices.map { tok =>
              val instAndMatchesOptSeq = findMatches(start, tok)

              td(style := borderStyle) {
                val spans = instAndMatchesOptSeq.flatMap { case (inst, matchesOpt) =>
                  val color = matchesOpt match {
                    case Some(true) => "green"
                    case Some(false) => "red"
                    case None => "gray"
                  }

                  Seq(
                    span(style := s"color: $color")(inst.getPosId.toString),
                    span(" ")
                  )
                }

                spans
              }
            }
          )
        }
      )
    ))
    val string = fragment.toString

    string
  }

  def mkHtmlTables(): Seq[String] = {
    val allSentences = transcript
        .map { debuggerRecord =>
          EqualityByIdentity(debuggerRecord.sentence)
        }
    val distinctSentences = allSentences.distinct
    val sentences = distinctSentences
        .map { equalityByIdentity => equalityByIdentity.any.asInstanceOf[Sentence] }
    val htmlTables = sentences.map(mkHtmlTable)

    htmlTables
  }


  def printHtmlTables(): Unit = {
    val htmlTable = mkHtmlTables()

    println(htmlTable)
  }


/*

  // Find all matching MatchTokens and say what they matched.
  def debugMatchTokens(): Unit = {
    val transcript = Debugger.instance.transcript
    val nameMatchTokenTokenSeq = transcript
        .filter { debuggerRecord =>
          debuggerRecord.matches && debuggerRecord.inst.isInstanceOf[MatchToken]
        }
        .map { debuggerRecord =>
          (debuggerRecord.extractor.name, debuggerRecord.inst.asInstanceOf[MatchToken], debuggerRecord.sentence.words(debuggerRecord.tok))
        }

    nameMatchTokenTokenSeq.foreach(println)
  }

  def debugDoneRules(): Unit = {
    val transcript = Debugger.instance.transcript
    val nameSentencestartTokSeq = transcript
        .filter { debuggerRecord =>
          debuggerRecord.matches && debuggerRecord.inst.isInstanceOf[Done.type]
        }
        .map { debuggerRecord =>
          (debuggerRecord.extractor.name, debuggerRecord.sentence, debuggerRecord.start, debuggerRecord.tok)
        }

    nameSentencestartTokSeq.foreach { case (name, sentence, start, tok) =>
      val words = sentence.words.clone
      words(start) = "[" + words(start)
      words(tok - 1) = words(tok - 1) + "]"

      println(s"""Rule $name matched sentence "${words.mkString(" ")}".""")
    }
  }

  def hasWidth(inst: Inst): Boolean = {
    inst.isInstanceOf[MatchToken] || inst.isInstanceOf[MatchMention] // What is the width of this?
  }

  def debugPartialMatches(): Unit = {
    val transcript = Debugger.instance.transcript
    val keyAndMinPosMaxPos = transcript
        .groupBy { debuggerRecord =>
          (debuggerRecord.document, debuggerRecord.loop, debuggerRecord.extractor, debuggerRecord.sentenceIndex, debuggerRecord.start)
        }
        .filter { case (key, debuggerRecords) =>
          // We need to have something not SaveStart that matched and nothing that is Done.
          // Some matches are zero-width and should be ignored.  Record this fact in the Inst.
          // Alternatively, highlight empty string somewhere.
          debuggerRecords.exists { debuggerRecord => !debuggerRecord.inst.isInstanceOf[SaveStart] && debuggerRecord.matches } &&
          !debuggerRecords.exists { debuggerRecord => debuggerRecord.inst.isInstanceOf[Done.type] && debuggerRecord.matches }
        }
        .map { case (key, debuggerRecords) =>
          val matchingToks = debuggerRecords.filter { debuggerRecord => debuggerRecord.matches && hasWidth(debuggerRecord.inst) }.map(_.tok)
          val minPos = key._5
          val maxPos = if (matchingToks.isEmpty) minPos else matchingToks.max + 1
          key -> (minPos, maxPos)
        }

    keyAndMinPosMaxPos.foreach { case ((document, loop, extractor, sentenceIndex, start), (minPos, maxPos)) =>
      val words = document.sentences(sentenceIndex).words.clone
      if (maxPos > minPos) {
        words(minPos) = ">" + words(minPos)
        words(maxPos - 1) = words(maxPos - 1) + "<"
      }
      else
        words(minPos) = "><" + words(minPos)

      println(s"""Rule ${extractor.name} partially matched sentence "${words.mkString(" ")}".""")
    }
  }

  debugMatchTokens()
  debugDoneRules()
  debugPartialMatches()
*/
}
