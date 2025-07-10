package org.clulab.processors.webapp2

import cask.MainRoutes
import com.typesafe.config.ConfigFactory
import io.undertow.Undertow
import io.undertow.server.handlers.BlockingHandler
import ujson.{Value => UjValue}
import scalatags.Text.all._

import scala.io.Source
import scala.util.Using

// This seems to require Java 11 because of Undertow.
class CaskServer(host: String, port: Int) extends MainRoutes {
  println("[processors] Initializing the processor ...")
  val informationExtractor = {
    val config = ConfigFactory.load("processors")

    new InformationExtractor(config, System.out)
  }
  println("[processors] Completed Initialization ...")

  val root = {
    val innerHtml = Using.resource(Source.fromResource("index.html")) { source =>
      source.mkString
    }

    doctype("html")(raw(innerHtml))
  }

  def toUjson(json: String): UjValue = {
    val ujValue =
      if (json.isEmpty) ujson.Str("") // TODO: ujson crashes if json is empty.  There is no JNothing.
      else ujson.read(json)

    ujValue
  }

  override def defaultHandler: BlockingHandler = {
    new BlockingHandler(
      new CorsHandler(
        dispatchTrie,
        mainDecorators,
        debugMode = false,
        handleNotFound,
        handleMethodNotAllowed,
        handleEndpointError
      )
    )
  }

  // The methods below marked @cask.OPTIONS are for CORS.

  @cask.get("/")
  def getRoot() = root

  @cask.get("/parseText")
  def parseText(text: String): UjValue = {
    println("Text:")
    println(text)
    println()

    val document = informationExtractor.annotate(text)

    println("Sentences:")
    document.sentences.foreach { sentence =>
      println(sentence.getSentenceText)
    }
    println()

    val mentions = informationExtractor.extract(document)

    println("Mentions:")
    mentions.foreach { mention =>
      informationExtractor.printMention(mention)
    }
    println()

    val json = informationExtractor.processDocument(text, document, mentions)
    val ujValue = toUjson(json)

    ujValue
  }

  @cask.staticResources("/assets")
  def assetRoute() = "."

  @cask.staticResources("/favicon.ico")
  def favIconRoute() = "./images"

  {
    initialize()
    val server = Undertow.builder
        .addHttpListener(port, host)
        .setHandler(defaultHandler)
        .build

    server.start()
  }
}
