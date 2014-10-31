package edu.arizona.sista.matcher

import scala.util.control.Breaks._
import scala.collection.mutable.ArrayBuffer
import edu.arizona.sista.processors.{Document, Sentence}
import edu.arizona.sista.struct.Interval

object NamedEntityExtractor {
  def getEntityMentions(document: Document): Seq[EntityMention] = for {
    (sentence, index) <- document.sentences.zipWithIndex
    (name, from, until) <- readLabels(entityLabels(sentence))
    tokenInterval = Interval(from, until)
  } yield new EntityMention(name, index, tokenInterval)

  private def entityLabels(sentence: Sentence): Seq[String] = sentence.entities match {
    case Some(labels) => labels
    case None => scala.sys.error("sentence has no entities")
  }

  // reads stanford style labels
  def readLabels(labels: Seq[String]): Seq[(String, Int, Int)] = {
    val mentions = new ArrayBuffer[(String, Int, Int)]

    var name: Option[String] = None
    var start: Option[Int] = None
    var end: Option[Int] = None

    for ((label, idx) <- labels.zipWithIndex if label != "O") {
      breakable {
        if (name.isDefined) {
          if (label == name.get && idx == end.get + 1) {
            // the mention keeps growing
            end = Some(idx)
            break
          } else if (label != name.get || idx > end.get + 1) {
            // a mention just ended
            val m = (name.get, start.get, end.get + 1)
            mentions += m
          }
        }
        // start new mention
        name = Some(label)
        start = Some(idx)
        end = Some(idx)
      }
    }

    // include last mention
    if (name.isDefined) {
      val m = (name.get, start.get, end.get + 1)
      mentions += m
    }

    mentions
  }

  // reads IOB style labels
  def readLabelsIOB(labels: Seq[String]): Seq[(String, Int, Int)] = {
    val mentions = new ArrayBuffer[(String, Int, Int)]

    var name: Option[String] = None
    var start: Option[Int] = None
    var end: Option[Int] = None

    for ((label, idx) <- labels.zipWithIndex if label != "O") {
      if (label startsWith "B-") {
        if (name.isDefined) {
          // a mention just ended
          val m = (name.get, start.get, end.get + 1)
          mentions += m
        }
        // start new mention
        name = Some(label.drop(2))
        start = Some(idx)
        end = Some(idx)
      } else if (label startsWith "I-") {
        // the mention keeps growing
        end = Some(idx)
      } else {
        scala.sys.error("tag not in IOB format")
      }
    }

    // include last mention
    if (name.isDefined) {
      val m = (name.get, start.get, end.get + 1)
      mentions += m
    }

    mentions
  }
}
