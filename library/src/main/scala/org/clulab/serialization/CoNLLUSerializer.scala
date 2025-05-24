package org.clulab.serialization

import org.clulab.processors.Document

import java.io.PrintWriter

object CoNLLUSerializer {
  val UNDEF = "_"
  val ROOT = "root"

  def getOrElseUndef(stringsOpt: Option[Seq[String]], i: Int): String =
      stringsOpt.map(_(i)).getOrElse(UNDEF)

  /**
   * Saves a doc in a format inspired by CoNLL-U
   * See the processors documentation page for details
   */
  def saveCoNLLUExtended(pw: PrintWriter, doc: Document): Unit = {
    for(sent <- doc.sentences) {
      for(i <- sent.indices) {
        val id = i + 1 // offsets start at 1 in CoNLL-U
        val form = sent.words(i)
        val lemma = getOrElseUndef(sent.lemmas, i)
        val upos = getOrElseUndef(sent.tags, i)
        val xpos = UNDEF
        val feats = UNDEF

        val (head, depRel) = sent.universalEnhancedDependencies.map { deps =>
          val edges = deps.getIncomingEdges(i)
          val (head, depRel) = edges.headOption.getOrElse((-1, ROOT))

          ((head + 1).toString, depRel)
        }.getOrElse((UNDEF, UNDEF))
        val deps = UNDEF

        // extra info specific to us
        val startOffset = sent.startOffsets(i)
        val endOffset = sent.endOffsets(i)
        val ent = getOrElseUndef(sent.entities, i)
        val entNorm = {
          val possiblyEmptyEntNorm = getOrElseUndef(sent.norms, i)
          val impossiblyEmptyEntNorm =
            if (possiblyEmptyEntNorm.isEmpty) UNDEF
            else possiblyEmptyEntNorm

          impossiblyEmptyEntNorm
        }
        val chunk = getOrElseUndef(sent.chunks, i)

        pw.println(s"$id\t$form\t$lemma\t$upos\t$xpos\t$feats\t$head\t$depRel\t$deps\t$startOffset\t$endOffset\t$ent\t$entNorm\t$chunk")
      }

      pw.println()
    }
  }
}
