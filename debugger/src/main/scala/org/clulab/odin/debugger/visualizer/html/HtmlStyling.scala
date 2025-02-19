package org.clulab.odin.debugger.visualizer.html

import scalatags.Text.all._

trait HtmlStyling {
  val bordered = "bordered"
  val green = "green"
  val red = "red"
  val gray = "gray"
  val right = "right"
  val wide = "wide"

  val style = tag("style")("""
    |body, table, span.nodeLabel p:first-line {
    |  font-family: system-ui, sans-serif;
    |  font-size: 12pt;
    |}
    |
    |table.bordered, table.bordered th, table.bordered td {
    |  border: 1px solid;
    |  border-collapse: collapse;
    |}
    |
    |td.right {
    |  text-align: right;
    |}
    |
    |td.wide {
    |  width: 100%;
    |}
    |
    |.green {
    |  color: green;
    |}
    |
    |.red {
    |  color: red;
    |}
    |
    |.gray {
    |  color: gray;
    |}
    |
    |span.nodeLabel p {
    |  font-size: 8pt;
    |}
    |
    |""".stripMargin
  )
}
