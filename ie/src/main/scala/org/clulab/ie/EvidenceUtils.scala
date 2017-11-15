package org.clulab.ie

import org.clulab.odin._


object EvidenceUtils {

  /***
    * Takes an Odin Mention (m) and produces a markup of the evidence span.
    * Creates "mention-arg", "mention-arg-$role" (for each argument),
    * "mention-trigger" (for each trigger),
    * "mention", and "mention-$label" (for total mention span) classes for spans
    */
  def formatEvidence(m: Mention): String = {

    val tag = "mark"
    val CLOSE = s"</$tag>"

    val toks: Array[String] = m.sentenceObj.words.clone()

    // markup args
    for {
      (role, arguments) <- m.arguments
      arg <- arguments
    } {
      // assign two classes
      val startIdx = arg.start
      val endIdx = arg.end - 1
      val OPEN = s"""<$tag class="mention-arg mention-arg-$role">"""
      toks(startIdx) = OPEN + toks(startIdx)
      toks(endIdx) = toks(endIdx) + CLOSE
    }

    // markup trigger (if present)
    m match {
      case em: EventMention =>
        val startIdx = em.trigger.start
        val endIdx = em.trigger.end - 1
        val OPEN = s"""<$tag class="mention-trigger">"""
        toks(startIdx) = OPEN + toks(startIdx)
        toks(endIdx) = toks(endIdx) + CLOSE
      case _ => ()
    }

    // markup total span
    toks(m.start) = s"""<$tag class="mention mention-${m.label}">""" + toks(m.start)
    toks(m.end - 1) = toks(m.end - 1) + CLOSE

    toks.mkString(" ")
  }
}