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
    |.collapser {
    |  background-color: #999;
    |  /*color: white;*/
    |  cursor: pointer;
    |  padding: 4px;
    |  margin: 4px;
    |  width: 100%;
    |  border: none;
    |  text-align: left;
    |  outline: none;
    |}
    |
    |.active, .collapser:hover {
    |  /*padding: 4px;
    |  margin: 4px;*/
    |  background-color: #777;
    |}
    |
    |.active.collapser:hover {
    |  background-color: #555;
    |}
    |
    |.collapsible {
    |  /*padding: 0 8px;*/
    |  margin: 8px;
    |  display: block;
    |  overflow: hidden;
    |  background-color: #f1f1f1;
    |}
    |""".stripMargin
  )
}
