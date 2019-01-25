package org.clulab.odin

import org.clulab.processors.{Document, RelationTriple}

package object openie {

  /**
    * Converts an instance of RelationTriple into an odin event mention
    * @param triple
    * @param sentence
    * @param doc
    * @return
    */
  def relationTripleToEventMention(triple:RelationTriple, sentence:Int, doc:Document):EventMention = {
    // Create text-bound annotations for the subject, object and relation (trigger)
    val subject = new TextBoundMention("OpenIEEntity", triple.subjectInterval, sentence, doc, true, foundBy = "OpenIE")
    val obj = new TextBoundMention("OpenIEEntity", triple.objectInterval, sentence, doc, true, foundBy = "OpenIE")
    val trigger = new TextBoundMention("OpenIETrigger", triple.relationInterval, sentence, doc, true, foundBy = "OpenIE")

    // Create and return and event mention with those roles
    new EventMention("OpenIEEvent",
      trigger,
      Map(
        "subject" -> Seq(subject),
        "object" -> Seq(obj)
      ),
      sentence,
      doc,
      true,
      "OpenIE"
    )
  }

  /**
    * Implicit wrapper that returns all the openie relations in a document as an array of event mentions
    * @param doc
    */
  implicit class RichDocument(val doc:Document) extends AnyVal {
    def openIEAsOdin:Array[EventMention] = doc.sentences.zipWithIndex.flatMap{
      case (sentence, ix) =>
        sentence.relations match {
          case Some(triples) => triples map (relationTripleToEventMention(_, ix, doc))
          case None => Array.empty[EventMention]
        }
    }
  }
}
