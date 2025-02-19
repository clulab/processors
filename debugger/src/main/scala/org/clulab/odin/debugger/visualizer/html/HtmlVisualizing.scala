package org.clulab.odin.debugger.visualizer.html

import scalatags.Text.all._
import scalatags.generic.Frag
import scalatags.text.Builder

trait HtmlVisualizing extends HtmlStyling {
  type Fragment = Frag[Builder, String]

  // Turn it into multiple table rows, preserving indentation.
  def toRows(lines: Seq[String], colCount: Int): Seq[Fragment] = {
    val rowFragments = lines.map { line =>
      val indent = line.takeWhile(_ == ' ')
      val rest = line.drop(indent.length)
      val fragment = frag(
        span(raw("&nbsp;" * indent.length)),
        span(rest)
      )

      frag(tr(
        td(colspan := colCount)(
          fragment
        )
      ))
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
        span(raw("&nbsp;" * indent.length)),
        span(rest)
      )

      if (index != lines.length - 1) spans :+ br()
      else spans
    }

    frag(tr(
      td(colspan := colCount)(
        spans
      )
    ))
  }

  def toSpans(lines: Seq[String]): Seq[Fragment] = {
    val spans = lines.zipWithIndex.flatMap { case (line, index) =>
      val indent = line.takeWhile(_ == ' ')
      val rest = line.drop(indent.length)
      val spans = Seq(
        span(raw("&nbsp;" * indent.length)),
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
