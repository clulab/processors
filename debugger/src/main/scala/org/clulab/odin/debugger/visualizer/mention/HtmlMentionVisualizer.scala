package org.clulab.odin.debugger.visualizer.mention

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.{MentionMatch, Transcript}
import org.clulab.odin.debugger.debug.finished.FinishedMention
import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import scalatags.Text.all._

case class FlatFinishedMention(
  mention: Mention,
  stateMention: Mention,
  mentionMatch: MentionMatch
)

class HtmlMentionVisualizer extends MentionVisualizer with HtmlVisualizing {

  def visualize(finishedMentions: Transcript[FinishedMention]): HtmlVisualization = {
    val flatFinishedMentions = finishedMentions.values.flatMap { finishedMention =>
      finishedMention.stateMentions.zip(finishedMention.mentionMatches).map { case (stateMention, mentionMatches) =>
        FlatFinishedMention(finishedMention.mention, stateMention, mentionMatches)
      }
    }
    val topRows = Seq(
      tr(
        th(rowspan := 2)(checkEmpty),
        th(colspan := 4)("Reference Mention"),
        th(rowspan := 2)(nbsp()),
        th(colspan := 4)("State Mention"),
        th(rowspan := 2)("Reason")
      ),
      tr(
        th("Rule"), th("Label"), th("Type"), th("Text"),
        th("Rule"), th("Label"), th("Type"), th("Text"),
      )
    )

    def mkTds(mention: Mention): Fragment = {
      val simpleName = mention.getClass.getSimpleName
      val rule = mention.foundBy
      val label = mention.label
      val text = mention.text

      frag(
        td(rule),
        td(label),
        td(simpleName),
        td(text)
      )
    }

    val botRows = flatFinishedMentions.map { flatFinishedMention =>
      tr(
        td(check(flatFinishedMention.mentionMatch.matches)),
        mkTds(flatFinishedMention.mention),
        td(),
        mkTds(flatFinishedMention.stateMention),
        td(flatFinishedMention.mentionMatch.reason)
      )
    }
    val tableFragment = table(`class` := bordered)(
      topRows,
      botRows
    )

    new HtmlVisualization(tableFragment)
  }
}
