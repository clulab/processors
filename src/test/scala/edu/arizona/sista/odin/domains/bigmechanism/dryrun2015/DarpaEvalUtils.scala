package edu.arizona.sista.odin.domains.bigmechanism.dryrun2015

import edu.arizona.sista.odin._

/**
 * Utility methods for the tests in this directory
 */
object DarpaEvalUtils {
  def hasEventWithArguments(label: String, args: Seq[String], mentions: Seq[Mention]): Boolean = {
    for (m <- mentions) {
      if (!m.isInstanceOf[TextBoundMention]) {
        if (m.labels contains label) {
          // found the label

          // This is only necessary because we decided to make complexes using relation mentions.
          // ex. GTP hydrolysis for Ras => "Ras-GTP" becomes the label of a resultant relation mention.
          // We really shouldn't be doing this sort of thing in a mention.
          val allText = s"${m.text} ${
            m.arguments.values.
              flatten
              .map(_.text)
              .mkString(" ")
          }".toLowerCase

          if (args.forall { arg => allText contains arg.toLowerCase}) {
            //println(s"\t==> found event mention: ${m.text}")
            return true
          }
        }
      }
    }
    false
  }

  def hasEntity(text: String, mentions: Seq[Mention]): Boolean = {
    for (m <- mentions) {
      if (m.isInstanceOf[TextBoundMention]) {
        val tm = m.asInstanceOf[TextBoundMention]
        if (tm.text == text) {
          //println(s"\t==> found entity mention: ${tm.text}")
          return true
        }
      }
    }
    false
  }

  def hasEntityWithSite(text: String, site: String, mentions: Seq[Mention]): Boolean = {
    for (m <- mentions) {
      if (m.isInstanceOf[RelationMention]) {
        val rm = m.asInstanceOf[RelationMention]
        if (rm.arguments.contains("site") &&
          contains(rm.arguments.get("site").get, site) &&
          rm.arguments.contains("protein") &&
          contains(rm.arguments.get("protein").get, text)) {
          //println(s"\t==> found entity mention with site: ${rm.text}")
          return true
        }
      }
    }
    false
  }

  def contains(mentions: Seq[Mention], text: String): Boolean = {
    for (m <- mentions) if (m.text == text) return true
    false
  }

  def hasPositiveRegulationByEntity(controllerEntity: String, controlledLabel: String, controlledArgs: Seq[String], mentions: Seq[Mention]): Boolean =
    hasRegulationByEntity("Positive_regulation", controllerEntity, controlledLabel, controlledArgs, mentions)

  def hasNegativeRegulationByEntity(controllerEntity: String, controlledLabel: String, controlledArgs: Seq[String], mentions: Seq[Mention]): Boolean =
    hasRegulationByEntity("Negative_regulation", controllerEntity, controlledLabel, controlledArgs, mentions)

  def hasRegulationByEntity(label: String,
                            controllerEntity: String,
                            controlledLabel: String,
                            controlledArgs: Seq[String],
                            mentions: Seq[Mention]): Boolean = {
    for (m <- mentions) {
      if (!m.isInstanceOf[TextBoundMention]) {
        if (m.labels contains label) {
          // found the regulation label
          val controller = m.arguments.get("controller")
          val controlled = m.arguments.get("controlled")

          if (controller.isDefined && controlled.isDefined && controlled.get.head.isInstanceOf[EventMention]) {
            // some obvious sanity checks
            val controlledEvent = controlled.get.head.asInstanceOf[EventMention]
            if (controller.get.head.text == controllerEntity && // found the controller entity
              controlledEvent.label == controlledLabel) {
              val allText = s"${m.text} ${controlledEvent.arguments.values
                .flatten
                .map(_.text)
                .mkString(" ")}".toLowerCase

              if (controlledArgs.forall{arg => allText contains arg.toLowerCase}) {
                //println(s"\t==> found event mention: ${m.text}")
                return true
              }
            }
          }
        }
      }
    }
    false
  }

  def header(name: String) {
    println(s"\n${":" * 20}$name${":" * 20}\n")
  }
}
