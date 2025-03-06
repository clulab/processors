package org.clulab.odin.debugger.odin

import org.clulab.odin.impl.ArgumentPattern
import org.clulab.odin.{Attachment, Mention, SynPath}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class DebuggingMention(label: String, override val arguments: Map[String, Seq[Mention]]) extends Mention {
  override val labels: Seq[String] = Seq(label)
  override val attachments: Set[Attachment] = Set.empty
  override val paths: Map[String, Map[Mention, SynPath]] = Map.empty
  override val foundBy: String = "Debugger"

  override def tokenInterval: Interval = ???
  override def sentence: Int = ???
  override def document: Document = ???
  override def keep: Boolean = ???

  override def text: String = ""
}

object DebuggingMention {

  def apply(label: String, argumentPatterns: Seq[ArgumentPattern]): DebuggingMention = {
    val names = argumentPatterns.map(_.name).distinct
    val arguments = names.map { name =>
      name -> Seq.empty
    }.toMap

    new DebuggingMention(label, arguments)
  }
}
