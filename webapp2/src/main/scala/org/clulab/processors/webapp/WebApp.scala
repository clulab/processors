package org.clulab.processors.webapp

import cats.effect._
import com.comcast.ip4s._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

object WebApp extends IOApp {
  object ParseTextQueryParamMatcher extends QueryParamDecoderMatcher[String]("text")

  def process(request: Request[IO])(block: => JValue): IO[Response[IO]] = {
    println()
    println(s" Enter: ${request.pathInfo}")
    val result = JsonMethods.pretty(block)
    println(s"Result: $result")
    println(s"  Exit: ${request.method}")
    val response = Ok(result)

    response
  }

  val webProcessor = new WebProcessor()
  val service = HttpRoutes.of[IO] {
    case request @ GET -> Root =>
      StaticFile.fromResource("/public/index.html", Some(request)).getOrElseF(NotFound())
    case request @ GET -> Root / "parseText" :? ParseTextQueryParamMatcher(text) =>
      process(request)(webProcessor.parseText(text))
    case request @ GET -> Root / "favicon.ico" =>
      StaticFile.fromResource("/public/images/favicon.ico", Some(request)).getOrElseF(NotFound())
    case request @ GET -> "assets" /: rest => // Note lack of root.
      StaticFile.fromResource(s"/public/$rest", Some(request)).getOrElseF(NotFound())
  }
  val app = Router("/" -> service).orNotFound
  val serverBuilder = EmberServerBuilder
      .default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(port"9000")
      .withHttpApp(app)

  override def run(args: List[String]): IO[ExitCode] = {
    serverBuilder
        .build
        .use(_ => IO.never)
        .as(ExitCode.Success)
  }
}
