package org.clulab.processors.webapp2.serialization

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.json4s.JValue
import org.json4s.JsonDSL._

class WebSerializer() {

  def processDocument(text: String, document: Document, mentions: Seq[Mention]): JValue = {
    val json = mkJson(text, document, mentions)

    json
  }

  def mkJson(text: String, document: Document, mentions: Seq[Mention]): JValue = {
    val sentence = document.sentences.head
    val syntaxJson = new SyntaxObj(document, text).mkJson
    val odinJson = new OdinObj(text, sentence, mentions).mkJson
    val mentionsHtml = new MentionsObj(mentions).mkHtml
    val parseHtml = new ParseObj(document).mkHtml

    ("syntax" -> syntaxJson) ~
    ("odin" -> odinJson) ~
    ("mentions" -> mentionsHtml) ~
    ("parse" -> parseHtml)
  }
}
