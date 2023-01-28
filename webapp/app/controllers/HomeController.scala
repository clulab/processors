package controllers

import com.typesafe.config.Config
import com.typesafe.config.ConfigRenderOptions

import javax.inject._
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.MaaSHandler
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidos.serialization.web.BuildInfoObj
import org.clulab.wm.eidos.serialization.web.WebSerializer
import org.clulab.wm.eidos.utils.{DisplayUtils, PlayUtils}
import play.api.mvc._
import play.api.libs.json._
import play.api.mvc.Action

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  // -------------------------------------------------
  // Initialize the EidosSystem
  // -------------------------------------------------
  println("[EidosSystem] Initializing the EidosSystem ...")
  val eidosConfig: Config = EidosSystem.defaultConfig
  val ieSystem: EidosSystem = new EidosSystem(eidosConfig)
  val webSerializer = new WebSerializer(ieSystem, eidosConfig)
  println("[EidosSystem] Completed Initialization ...")

  {
    println("[EidosSystem] Priming the EidosSystem ...")
    // This is essentially processPlayText() but with some different options.
    val annotatedDocument = ieSystem.extractFromText(
      "In 2014 drought caused a famine in Ethopia.",
      cagRelevantOnly = true, Some("2019-08-09")
    )
    val corpus = new JLDCorpus(annotatedDocument)
    corpus.serialize()
    println("[EidosSystem] Completed Priming ...")
  }

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def buildInfo: Action[AnyContent] = Action {
    Ok(BuildInfoObj.mkJson)
  }

  def config: Action[AnyContent] = Action {
    val options: ConfigRenderOptions = ConfigRenderOptions.concise.setFormatted(true).setJson(true)
    val jsonString = eidosConfig.root.render(options)

    Ok(jsonString).as(JSON)
  }

  // -------------------------------------------
  // API entry points for MaaS
  // -------------------------------------------
  def mapNode: Action[AnyContent] = Action { request =>
    val data = request.body.asJson.get.toString()

    // Note -- topN can be exposed to the API if needed
    Ok(MaaSHandler.mapNodeToPrimaryConcepts(ieSystem, data, topN = 10)).as(JSON)
  }

  def mapOntology: Action[AnyContent] = Action { request =>
    val fileContents = request.body.asText.get

    // Note -- topN can be exposed to the API if needed
    Ok(MaaSHandler.mapOntology(ieSystem, "MaaS", fileContents, topN = 10)).as(JSON)
  }

  // Entry method
  def parseText(text: String, cagRelevantOnly: Boolean): Action[AnyContent] = Action {
    println(s"Processing sentence: $text")
    println()

    val annotatedDocument = ieSystem.extractFromText(text, cagRelevantOnly)
    val doc = annotatedDocument.document
    val eidosMentions = annotatedDocument.eidosMentions
    val sortedEidosMentions = eidosMentions.sortBy { eidosMention =>
      val m = eidosMention.odinMention
      (m.sentence, m.getClass.getSimpleName)
    }

    println(s"Tokenized sentence: ${doc.sentences.head.getSentenceText}")
    println()
    println("Mention texts:")
    sortedEidosMentions.foreach(eidosMention => println(eidosMention.odinMention.text))
    println()
    println("Mention details:")
    sortedEidosMentions.foreach(eidosMention => DisplayUtils.displayEidosMention(eidosMention))
    println()

    val json = webSerializer.processAnnotatedDocument(text, annotatedDocument)

    Ok(json)
  }

  // Webservice functions
  def process_text: Action[JsValue] = Action(parse.json) { request =>
    (request.body \ "text").asOpt[String].map { text =>
      println(s"Processing sentence: $text")
      val annotatedDocument = ieSystem.extractFromText(text)
      val corpus = new JLDCorpus(annotatedDocument)
      val jCorpus = corpus.serialize()
      val jsCorpus = PlayUtils.toPlayJson(jCorpus)

      Ok(jsCorpus)
    }.getOrElse {
      BadRequest("Missing parameter [text]")
    }
  }

  def reground: Action[JsValue] = Action(parse.json) { request =>

    def extract(name: String): JsLookupResult = request.body \ name

    try {
      val name = extract("name").asOpt[String].getOrElse("Custom")
      val ontologyYaml = extract("ontologyYaml").as[String]
      val texts = extract("texts").as[JsArray].value.map { jsString =>
        (jsString: @unchecked) match {
          case JsString(text) => text
        }
      }
      val filter = extract("filter").asOpt[Boolean].getOrElse(true)
      val topk = extract("topk").asOpt[Int].getOrElse(10)
      val isAlreadyCanonicalized = extract("isAlreadyCanonicalized").asOpt[Boolean].getOrElse(true)

      try {
        val ontologyHandler = ieSystem.components.ontologyHandlerOpt.get
        val regroundings = ontologyHandler.reground(name, ontologyYaml, texts, filter, topk, isAlreadyCanonicalized)
        val result = JsArray { regroundings.map { regrounding =>
            JsArray { regrounding.map { case (grounding, score) =>
                JsObject(Map(
                  "grounding" -> JsString(grounding),
                  "score" -> JsNumber(score.toDouble)
                ))
            }}
        }}

        Ok(result)
      }
      catch {
        case throwable: Throwable =>
          InternalServerError(JsString(s"The server couldn't handle it: ${throwable.getMessage}"))
      }
    }
    catch {
      case throwable: Throwable =>
        BadRequest(JsString(s"The request seems to be bad: ${throwable.getMessage}"))
    }
  }
}
