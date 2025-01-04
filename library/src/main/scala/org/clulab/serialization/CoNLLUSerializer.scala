package org.clulab.serialization

import org.clulab.processors.Document

import java.io.PrintWriter

object CoNLLUSerializer {
  val UNDEF = "_"
  val ROOT = "root"

  /**
   * Saves a doc in a format inspired by CoNLL-U
   * See the processors documentation page for details
   */
  def saveCoNLLUExtended(pw: PrintWriter, doc: Document): Unit = {
    for(sent <- doc.sentences) {
      for(i <- 0 until sent.size) {
        val id = i + 1 // offsets start at 1 in CoNLL-U
        val form = sent.words(i)
        val lemma = if(sent.lemmas.isDefined) sent.lemmas.get(i) else UNDEF
        val upos = if(sent.tags.isDefined) sent.tags.get(i) else UNDEF
        val xpos = UNDEF
        val feats = UNDEF

        val (head, depRel) =
          if(sent.universalEnhancedDependencies.isDefined) {
            val deps = sent.universalEnhancedDependencies.get
            val edges = deps.getIncomingEdges(i)
            if(edges.length > 0) {
              ((edges.head._1 + 1).toString, edges.head._2)
            } else {
              (0, ROOT)
            }
          } else {
            (UNDEF, UNDEF)
          }
        val deps = UNDEF

        // extra info specific to us
        val startOffset = sent.startOffsets(i)
        val endOffset = sent.endOffsets(i)
        val ent = if(sent.entities.isDefined) sent.entities.get(i) else UNDEF
        val entNorm = if(sent.norms.isDefined && sent.norms.get(i).nonEmpty) sent.norms.get(i) else UNDEF
        val chunk = if(sent.chunks.isDefined) sent.chunks.get(i) else UNDEF

        pw.println(s"$id\t$form\t$lemma\t$upos\t$xpos\t$feats\t$head\t$depRel\t$deps\t$startOffset\t$endOffset\t$ent\t$entNorm\t$chunk")
      }

      pw.println()
    }
  }
}
