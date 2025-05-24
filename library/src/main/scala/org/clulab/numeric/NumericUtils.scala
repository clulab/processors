package org.clulab.numeric

import org.clulab.numeric.actions.NumericActions
import org.clulab.numeric.mentions.Norm
import org.clulab.odin.{EventMention, Mention}
import org.clulab.processors.Document
import org.clulab.struct.Interval

import _root_.scala.util.control.Breaks._

object NumericUtils {
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
  def mkLabelsAndNorms(doc: Document, mentions: Seq[Mention]): (Seq[Seq[String]], Seq[Seq[String]]) = {
    val allEntities = doc.sentences.map { sentence =>
      sentence.entities.getOrElse(Seq.fill(sentence.size)("O"))
    }
    val allNorms = doc.sentences.map { sentence =>
      sentence.norms.getOrElse(Seq.fill(sentence.size)(""))
    }

    for (mention <- mentions) {
      if (NumericActions.isNumeric(mention) && mention.isInstanceOf[Norm]) {
        val sentenceIndex = mention.sentence
        val entities = allEntities(sentenceIndex)
        val norms = allNorms(sentenceIndex)

        addLabelsAndNorms(mention.asInstanceOf[Norm], entities, norms, mention.tokenInterval)
        removeOneEntityBeforeAnother(entities, norms, "B-LOC", "MEASUREMENT-LENGTH")
      }
    }

    (allEntities, allNorms)
  }

  def removeOneEntityBeforeAnother(entities: Seq[String], norms: Seq[String], triggerEntity: String, toBeRemovedShortened: String): Unit = {
    // removes entities and norms for unallowable entity sequences, e.g., don't extract 'in' as 'inch' before B-LOC in '... Sahal 108 in Senegal'
    // toBeRemovedShortened is entity without BIO-
    val zippedEntities = entities.zipWithIndex

    zippedEntities.foreach { case (outerEntity, outerIndex) =>
      if (outerIndex > 0 && outerEntity == triggerEntity && entities(outerIndex - 1).endsWith(toBeRemovedShortened)) {
        // Go in reverse replacing indices and norms in the immediate preceding mention.
        zippedEntities.slice(0, outerIndex).reverse
        breakable { // TODO: rewrite
          for ((innerEntity, innerIndex) <- zippedEntities.slice(0, outerIndex).reverse) {
            if (innerEntity.endsWith(toBeRemovedShortened)) {
              entities(innerIndex) = "O"
              norms(innerIndex) = ""
            } else break()
          }
        }
      }
    }
  }

  // TODO: These need to be mutable
  private def addLabelsAndNorms(m: Norm, entities: Seq[String], norms: Seq[String], tokenInt: Interval): Unit = {
    val label = m.neLabel
    val norm = m.neNorm

    // careful here: we may override some existing entities and norms
    // but, given that the numeric entity rules tend to be high precision, this is probably Ok...
    tokenInt.headOption.foreach { index =>
      entities(index) = "B-" + label
      norms(index) = norm
    }
    tokenInt.tail.foreach { index =>
      entities(index) = "I-" + label
      norms(index) = norm
    }
  }
}
