package org.clulab.processors.webapp2

object WebApp2 extends App {
  val host = "localhost"
  val port = 9000

  new CaskServer(host, port)
}
