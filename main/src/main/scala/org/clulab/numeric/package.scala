package org.clulab

import org.clulab.numeric.actions.NumericActions
import org.clulab.numeric.mentions.{DateMention, DateRangeMention, MeasurementMention, Norm, PercentageMention}
import org.clulab.odin.{EventMention, Mention}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval
import _root_.scala.util.control.Breaks._

package object numeric {
  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions.groupBy(_.sentence).map { case (sentence, mentions) =>
      sentence -> mentions.sortBy(_.start)
    }.withDefaultValue(Nil)
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText)
      println("Tokens: " + s.words.indices.zip(s.words).zip(s.tags.get).mkString(", "))
      s.tags foreach (x => println("Tags: " + x.mkString(", ")))
      s.entities foreach (x => println("Entities: " + x.mkString(", ")))
      s.norms foreach (x => println("Norms: " + x.mkString(", ")))
      println()

      val sortedMentions = mentionsBySentence(i).sortBy(_.label)
      sortedMentions foreach displayMention
      println()
    }
  }

  def displayMention(mention: Mention): Unit = {
    val boundary = s"\t${"-" * 30}"
    println(s"${mention.labels} => ${mention.text}")
    println(boundary)
    println(s"\tRule => ${mention.foundBy}")
    val mentionType = mention.getClass.toString.split("""\.""").last
    println(s"\tType => $mentionType")
    println(s"\tInterval => ${mention.tokenInterval}")
    mention match {
      case norm: Norm =>
        println(s"\tNorm => ${norm.neNorm}")
        println(s"\tNE => ${norm.neLabel}")
      case _ =>
    }
    println(boundary)
    if (mention.arguments.nonEmpty) {
      println("\tArgs:")
      mention match {
        case em: EventMention =>
          println(s"\ttrigger: ${em.trigger}")
          displayArguments(em)
        case _ =>
          displayArguments(mention)
      }
      println(s"$boundary")
    }
    println()
  }

  def displayArguments(b: Mention): Unit = {
    b.arguments foreach {
      case (argName, ms) =>
        ms foreach { v =>
          println(s"\t  * $argName ${v.labels.mkString("(", ", ", ")")} => ${v.text}")
        }
    }
  }

  /**
    * Sets the entities and norms fields in each Sentence based on the given numeric mentions
    * @param doc This document is modified in place
    * @param mentions The numeric mentions previously extracted
    */
  def setLabelsAndNorms(doc: Document, mentions: Seq[Mention]): Unit = {
    //
    // initialize entities and norms
    //
    for(s <- doc.sentences) {
      if(s.entities.isEmpty) {
        s.entities = Some(new Array[String](s.size))
        for(i <- s.entities.get.indices) {
          s.entities.get(i) = "O"
        }
      }
      if(s.norms.isEmpty) {
        s.norms = Some(new Array[String](s.size))
        for(i <- s.norms.get.indices) {
          s.norms.get(i) = ""
        }
      }
    }

    //
    // convert numeric entities to entity labels and norms
    //
    for(mention <- mentions) {
      if(NumericActions.isNumeric(mention) && mention.isInstanceOf[Norm]) {
        addLabelsAndNorms(mention.asInstanceOf[Norm], mention.sentenceObj, mention.tokenInterval)
      }
    }
    removeOneEntityBeforeAnother(doc, "B-LOC", "MEASUREMENT")
  }

  def removeOneEntityBeforeAnother(doc: Document, triggerEntity: String, toBeRemovedShortened: String): Unit = {
    // removes entities and norms for unallowable entity sequences, e.g., don't extract 'in' as 'inch' before B-LOC in '... Sahal 108 in Senegal'
    // toBeRemovedShortened is entity without BIO-
    for(s <- doc.sentences) {
      val zippedEntities = s.entities.get.zipWithIndex
      for ((e, i) <- zippedEntities) {
        if (i > 0 && e == triggerEntity && s.entities.get(i-1).endsWith(toBeRemovedShortened)) {
          s.entities.get(i - 1) = "O"
          // go in reverse replacing indices and norms in the immediate preceding mention
          breakable {
            for ((en, j) <- zippedEntities.slice(0, i ).reverse) {
              if (en.endsWith(toBeRemovedShortened)) {
                s.entities.get(j) = "O"
                s.norms.get(j) = ""
              } else break()
            }
          }
        }
      }
    }
  }

  private def addLabelsAndNorms(m: Norm, s: Sentence, tokenInt: Interval): Unit = {
    var first = true
    val norm = m.neNorm
    // careful here: we may override some existing entities and norms
    // but, given that the numeric entity rules tend to be high precision, this is probably Ok...
    for(i <- tokenInt.indices) {
      val prefix = if(first) "B-" else "I-"
      s.entities.get(i) = prefix + m.neLabel
      s.norms.get(i) = norm
      first = false
    }
  }
}
