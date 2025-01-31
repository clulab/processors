package org.clulab.odin.debugger.inspector

import org.clulab.odin.debugger.DebuggerRecord

import scala.collection.mutable

class Inspector(transcript: mutable.Buffer[DebuggerRecord]) {
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
