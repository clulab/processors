package org.clulab.odin.debugger.odin

import org.clulab.odin.{Attachment, Mention, SynPath}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class DebuggingMention(label: String) extends Mention {
  override val labels: Seq[String] = Seq(label)
  override val arguments: Map[String, Seq[Mention]] = Map.empty
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

  def apply(label: String): DebuggingMention = new DebuggingMention(label)
}
