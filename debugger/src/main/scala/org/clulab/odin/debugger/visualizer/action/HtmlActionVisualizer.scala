package org.clulab.odin.debugger.visualizer.action

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.{FinishedGlobalAction, FinishedLocalAction}
import org.clulab.odin.debugger.utils.EqualityByIdentity
import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import scalatags.Text.all._

import scala.collection.mutable

class HtmlActionVisualizer extends ActionVisualizer with HtmlVisualizing {

  def visualize(inMentions: Seq[Mention], outMentions: Seq[Mention]): HtmlVisualization = {
    val inEqualityMentions = inMentions.map(EqualityByIdentity(_)).distinct
    val outEqualityMentions = outMentions.map(EqualityByIdentity(_)).distinct
    val (boths, lefts) = inEqualityMentions.partition { inEqualityMention =>
      outEqualityMentions.contains(inEqualityMention)
    }
    val rights = outEqualityMentions.filterNot { outEqualityMention =>
      inEqualityMentions.contains(outEqualityMention)
    }
    val leftPairs = lefts.map { left => (Some(left.value), None) }
    val bothPairs = boths.map { both => (Some(both.value), Some(both.value)) }
    val rightPairs = rights.map { right => (None, Some(right.value)) }
    val allPairs = leftPairs ++ bothPairs ++ rightPairs
    val topRows = Seq(
      tr(
        th(colspan := 4)("Incoming Mention"),
        th(rowspan := 2)(nbsp()),
        th(colspan := 4)("Outgoing Mention")
      ),
      tr(
        th("Rule"), th("Label"), th("Type"), th("Text"),
        th("Rule"), th("Label"), th("Type"), th("Text")
      )
    )

    def mkTds(mentionOpt: Option[Mention]): Fragment = {
      val simpleName = mentionOpt.map(_.getClass.getSimpleName).getOrElse("")
      val rule = mentionOpt.map(_.foundBy).getOrElse("")
      val label = mentionOpt.map(_.label).getOrElse("")
      val text = mentionOpt.map(_.text).getOrElse("")

      frag(
        td(rule),
        td(label),
        td(simpleName),
        td(text)
      )
    }

    val botRows = allPairs.map { case (inMentionOpt, outMentionOpt) =>
      tr(
        mkTds(inMentionOpt),
        td(),
        mkTds(outMentionOpt)
      )
    }
    val tableFragment = table(`class` := bordered)(
      topRows,
      botRows
    )

    new HtmlVisualization(tableFragment)
  }

  override def visualizeLocal(transcript: Seq[FinishedLocalAction]): HtmlVisualization = {
    val inMentions = transcript.flatMap { finishedLocalAction =>
      finishedLocalAction.inMentions
    }
    val outMentions = transcript.flatMap { finishedLocalAction =>
      finishedLocalAction.outMentions
    }
     visualize(inMentions, outMentions)
  }

  override def visualizeGlobal(transcript: Seq[FinishedGlobalAction]): HtmlVisualization = {
    val inMentions = transcript.flatMap { finishedGlobalAction =>
      finishedGlobalAction.inMentions
    }
    val outMentions = transcript.flatMap { finishedGlobalAction =>
      finishedGlobalAction.outMentions
    }
    visualize(inMentions, outMentions)
  }
}
