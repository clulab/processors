package org.clulab

import org.clulab.odin.{Mention, TextBoundMention}
import org.clulab.processors.Document

package object numeric {
  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText)
      println("Tokens: " + (s.words.indices, s.words, s.tags.get).zipped.mkString(", "))
      s.tags foreach (x => println("Tags: " + x.mkString(", ")))
      s.entities foreach (x => println("Entities: " + x.mkString(", ")))
      s.norms foreach (x => println("Norms: " + x.mkString(", ")))
      println

      val sortedMentions = mentionsBySentence(i).filter(_.isInstanceOf[TextBoundMention]).sortBy(_.label)
      sortedMentions foreach displayMention
      println
    }
  }

  def displayMention(mention: Mention) {
    val boundary = s"\t${"-" * 30}"
    println(s"${mention.labels} => ${mention.text}")
    println(boundary)
    println(s"\tRule => ${mention.foundBy}")
    val mentionType = mention.getClass.toString.split("""\.""").last
    println(s"\tType => $mentionType")
    println(boundary)
    mention match {
      case tb: TextBoundMention =>
        println(s"\t${tb.labels.mkString(", ")} => ${tb.text}")
        displayArguments(tb)
      case _ => ()
    }
    println(s"$boundary\n")
  }

  def displayArguments(b: Mention): Unit = {
    b.arguments foreach {
      case (argName, ms) =>
        ms foreach { v =>
          println(s"\t$argName ${v.labels.mkString("(", ", ", ")")} => ${v.text}")
        }
    }
  }
}
