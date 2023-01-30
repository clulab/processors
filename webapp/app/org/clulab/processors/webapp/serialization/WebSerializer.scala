package org.clulab.processors.webapp.serialization

import org.clulab.odin.Mention
import org.clulab.processors.Document
import play.api.libs.json.{JsValue, Json}

class WebSerializer() {

  def processDocument(text: String, document: Document, mentions: Seq[Mention]): JsValue = {
    val json = mkJson(text, document, mentions)

    json
  }

  def mkJson(text: String, document: Document, mentions: Seq[Mention]): JsValue = {
    val sentence = document.sentences.head
    val syntaxJson = new SyntaxObj(document, text).mkJson
    val odinJson = new OdinObj(text, sentence, mentions).mkJson
    val mentionsHtml = new MentionsObj(mentions).mkHtml
    val parseHtml = new ParseObj(document).mkHtml

    Json.obj(fields =
      "syntax" -> syntaxJson,
      "odin" -> odinJson,
      "mentions" -> mentionsHtml,
      "parse" -> parseHtml
    )
  }
}
