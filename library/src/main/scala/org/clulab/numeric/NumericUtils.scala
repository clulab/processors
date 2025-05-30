package org.clulab.numeric

import org.clulab.numeric.actions.NumericActions
import org.clulab.numeric.mentions.Norm
import org.clulab.odin.{EventMention, Mention}
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.utils.WrappedArraySeq

import scala.collection.mutable

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
    val pertinentMentions = mentions.collect {
      case mention: Norm if NumericActions.isNumeric(mention) => mention
    }
    val mentionsBySentenceIndex = pertinentMentions.groupBy { mention => mention.sentence }
    val zippedLabelsAndNorms = doc.sentences.zipWithIndex.map { case (sentence, index) =>
      val mentions = mentionsBySentenceIndex.getOrElse(index, Seq.empty)

      if (mentions.isEmpty) {
        val entities = sentence.entities.getOrElse(WrappedArraySeq(Array.fill(sentence.size)("O")).toImmutableSeq)
        val norms = sentence.norms.getOrElse(WrappedArraySeq(Array.fill(sentence.size)("")).toImmutableSeq)

        (entities, norms)
      }
      else {
        val mutableEntities = sentence.entities
            .map { entities => Array(entities: _*) }
            .getOrElse(Array.fill(sentence.size)("O"))
        val mutableNorms = sentence.norms
            .map { norms => Array(norms: _*) }
            .getOrElse(Array.fill(sentence.size)(""))

        mentions.foreach { mention =>
          addLabelsAndNorms(mention.neLabel, mention.neNorm, mention.tokenInterval, mutableEntities, mutableNorms)
        }
        removeOneEntityBeforeAnother(mutableEntities, mutableNorms, "B-LOC", "MEASUREMENT-LENGTH")

        val immutableEntities = WrappedArraySeq(mutableEntities).toImmutableSeq
        val immutableNorms = WrappedArraySeq(mutableNorms).toImmutableSeq
        (immutableEntities, immutableNorms)
      }
    }
    val unzippedLabelsAndNorms = zippedLabelsAndNorms.unzip

    unzippedLabelsAndNorms
  }

  def removeOneEntityBeforeAnother(entities: mutable.Seq[String], norms: mutable.Seq[String], triggerEntity: String, toBeRemovedShortened: String): Unit = {
    var triggered = false

    entities.indices.reverse.foreach { index =>
      val entity = entities(index)

      if (entity == triggerEntity)
        triggered = true
      else {
        if (triggered)
          if (entity.endsWith(toBeRemovedShortened)) {
            entities(index) = "O"
            norms(index) = ""
          }
          else
            triggered = false
      }
    }
  }

  private def addLabelsAndNorms(label: String, norm: String, tokenInt: Interval, entities: mutable.Seq[String], norms: mutable.Seq[String]): Unit = {
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
