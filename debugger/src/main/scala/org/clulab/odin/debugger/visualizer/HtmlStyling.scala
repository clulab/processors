package org.clulab.odin.debugger.visualizer

import scalatags.Text.all._

trait HtmlStyling {
  val bordered = "bordered"
  val green = "green"
  val red = "red"
  val gray = "gray"
  val right = "right"

  val style = tag("style")("""
    |body {
    |  font-family: system-ui, sans-serif;
    |  font-size: 12px;
    |}
    |
    |table {
    |  font-size: 12px;
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
    |""".stripMargin
  )
}
