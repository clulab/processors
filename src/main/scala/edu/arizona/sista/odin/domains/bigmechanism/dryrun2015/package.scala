package edu.arizona.sista.odin.domains.bigmechanism

import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

package object dryrun2015 {

  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText())
      println
      mentionsBySentence(i).sortBy(_.label) foreach displayMention
      println("=" * 50)
    }
  }

  def displayMention(mention: Mention) {
    val boundary =  s"\t${"-" * 30}"
    println(mention.labels)
    println(boundary)
    println(s"\tRule => ${mention.foundBy}")
    println(s"\tType => ${mention.getClass.toString.split("""\.""").last}")
    println(boundary)
    mention match {
      case m: TextBoundMention =>
        println(s"\t${m.labels} => ${m.text}")

      case m: EventMention =>
        println(s"\ttrigger => ${m.trigger.text}")
        m.arguments foreach {
          case (k, vs) => for (v <- vs) println(s"\t$k (${v.labels}) => ${v.text}")
        }

      case m: RelationMention =>
        m.arguments foreach {
          case (k, vs) => for (v <- vs) println(s"\t$k (${v.labels}) => ${v.text}")
        }

      case _ => ()
    }
    println(s"$boundary\n")
  }

  // generates a representation of the mention that can be used
  // for the csv file expected by darpa
  implicit class Repr(mention: Mention) {
    def repr: String = mention match {
      case m: TextBoundMention => s"${m.labels}(${m.text})"
      case m: EventMention => s"${m.labels}(${dumpArgs(m.arguments)})"
      case m: RelationMention => s"${m.labels}(${dumpArgs(m.arguments)}"
    }

    private def dumpArgs(arguments: Map[String, Seq[Mention]]): String =
      arguments.map{ case (k, v) => s"$k=${dumpArgVal(v)}" }.mkString(", ")

    private def dumpArgVal(mentions: Seq[Mention]): String =
      if (mentions.size == 1) mentions(0).repr
      else s"[${mentions.map(_.repr).mkString(", ")}]"
  }
}
