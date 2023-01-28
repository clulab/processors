package controllers

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
    val mentionsJson = new MentionsObj(text, sentence, mentions).mkJson
    val parseHtml = new ParseObj(document).mkHtml

    Json.obj(fields =
      "syntax" -> syntaxJson,
      "mentions" -> mentionsJson,
      "parse" -> parseHtml
    )
  }
}
