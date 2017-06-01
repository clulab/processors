package org.clulab.serialization.json

import org.json4s._


trait JSONSerialization {

  def jsonAST: JValue

  def json(pretty: Boolean = false): String = stringify(jsonAST, pretty)

}