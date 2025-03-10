package org.clulab.odin.debugger.visualizer.html

import org.clulab.odin.debugger.visualization.HtmlFragment
import scalatags.Text.all._

trait HtmlVisualizing extends HtmlStyling {
  type Fragment = HtmlFragment.Fragment

  val checkTrue: Fragment = span(`class` := green)(raw("&#9745;"))
  val checkFalse: Fragment = span(`class` := red)(raw("&#9746;"))
  val checkEmpty: Fragment = raw("&#9744;")

  def check(value: Boolean): Fragment = if (value) checkTrue else checkFalse

  val nbspString = "&nbsp;"
  val rawNbsp1 = raw(nbspString * 1)
  val rawNbsp2 = raw(nbspString * 2)

  def nbsp(count: Int = 1): Fragment = {
    if (count == 1) rawNbsp1
    else if (count == 2) rawNbsp2
    else raw(nbspString * count)
  }

  // Turn it into multiple table rows, preserving indentation.
  def toRows(lines: Seq[String], colCount: Int): Seq[Fragment] = {
    val rowFragments = lines.map { line =>
      val indent = line.takeWhile(_ == ' ')
      val rest = line.drop(indent.length)
      val fragment = frag(
        span(nbsp(indent.length)),
        span(rest)
      )

      tr(
        td(colspan := colCount)(
          fragment
        )
      )
    }

    rowFragments
  }

  // Turn it into a single table row, preserving indentation.
  // Each line should be separated from others by a <br>.
  def toRow(lines: Seq[String], colCount: Int): Fragment = {
    val spans = lines.zipWithIndex.flatMap { case (line, index) =>
      val indent = line.takeWhile(_ == ' ')
      val rest = line.drop(indent.length)
      val spans = Seq(
        span(nbsp(indent.length)),
        span(rest)
      )

      if (index != lines.length - 1) spans :+ br()
      else spans
    }

    tr(
      td(colspan := colCount)(
        spans
      )
    )
  }

  def toSpans(lines: Seq[String]): Seq[Fragment] = {
    val spans = lines.zipWithIndex.flatMap { case (line, index) =>
      val indent = line.takeWhile(_ == ' ')
      val rest = line.drop(indent.length)
      val spans = Seq(
        span(nbsp(indent.length)),
        span(rest)
      )

      if (index != lines.length - 1) spans :+ br()
      else spans
    }

    spans
  }

  def toSpans(line: String): Seq[Fragment] =
      toSpans(line.linesIterator.toSeq)
}
