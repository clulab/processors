package org.clulab.processors.webapp2

import cask.Request
import cask.internal.DispatchTrie
import cask.main.{Main, Routes}
import cask.model.Response
import cask.router.{Decorator, EndpointMetadata, Result}
import cask.util.Logger
import io.undertow.server.HttpServerExchange
import io.undertow.util.HttpString

import scala.jdk.CollectionConverters._

class CorsHandler(
  dispatchTrie: DispatchTrie[Map[String, (Routes, EndpointMetadata[_])]],
  mainDecorators: Seq[Decorator[_, _, _, _]],
  debugMode: Boolean,
  handleNotFound: (Request) => Response.Raw,
  handleMethodNotAllowed: (Request) => Response.Raw,
  handleError: (Routes, EndpointMetadata[_], Result.Error, Request) => Response.Raw
)(implicit logger: Logger) extends Main.DefaultHandler(
  dispatchTrie,
  mainDecorators,
  debugMode,
  handleNotFound,
  handleMethodNotAllowed,
  handleError
)(logger) {

  override def handleRequest(exchange: HttpServerExchange): Unit = {
    exchange.getResponseHeaders
        .put(CorsHandler.accessControlAllowOrigin, CorsHandler.origin)
        .put(CorsHandler.accessControlAllowCredentials, CorsHandler.accepted)
        .putAll(CorsHandler.acccessControlAllowHeaders, CorsHandler.headers)
        .putAll(CorsHandler.accessControlAllowMethods, CorsHandler.methods)
    super.handleRequest(exchange)
  }
}

object CorsHandler {
  val accessControlAllowOrigin = new HttpString("Access-Control-Allow-Origin")
  val accessControlAllowCredentials = new HttpString("Access-Control-Allow-Credentials")
  val acccessControlAllowHeaders = new HttpString("Access-Control-Allow-Headers")
  val accessControlAllowMethods = new HttpString("Access-Control-Allow-Methods")

  val origin = "*"
  val accepted = "true"
  val headers = Set("Authorization", "Content-Type", "X-Requested-With").asJava
  val methods = Set("POST", "GET", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS").asJava
}
