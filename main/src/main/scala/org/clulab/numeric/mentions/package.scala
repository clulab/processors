package org.clulab.numeric

import org.clulab.odin.{Mention, RelationMention, TextBoundMention}

package object mentions {
  implicit class MentionOps(mention: Mention) {

    def toDateMention: DateMention =  mention match {
      case m: DateMention => m

      case m: RelationMention =>
        new DateMention(
          m.labels,
          m.tokenInterval,
          m.arguments,
          m.paths,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy,
          m.attachments)

      case m =>
        throw new RuntimeException(s"ERROR: cannot convert mention of type ${m.getClass.toString} to DateMention!")
    }

  }
}
