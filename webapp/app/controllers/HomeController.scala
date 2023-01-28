package controllers

import org.clulab.odin.ExtractorEngine
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import play.api.mvc._
import play.api.mvc.Action

import javax.inject._

@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
  println("[processors] Initializing the processor ...")

  val webSerializer = new WebSerializer()
  val customLexiconNer = LexiconNER(Seq.empty, Seq.empty, None)
  val processor = new CluProcessor(optionalNER = Some(customLexiconNer))
  val rules = """
    |taxonomy:
    |  - Entity:
    |    - Food
    |    - Person
    |  - Event:
    |    - Eating
    |
    |rules:
    |  - name: foods-from-lexicon
    |    priority: "1"
    |    label: Food
    |    type: token
    |    pattern: |
    |      [entity='B-FOOD'] [entity='I-FOOD']*
    |
    |  - name: person-from-lexicon
    |    priority: "1"
    |    label: Person
    |    type: token
    |    pattern: |
    |      [entity='B-PER'] [entity='I-PER']*
    |
  """.stripMargin
  val extractorEngine: ExtractorEngine = ExtractorEngine(rules)

  {
    val document = processor.annotate("John eats cake.")
    extractorEngine.extractFrom(document)
  }
  println("[processors] Completed Initialization ...")

  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def parseText(text: String): Action[AnyContent] = Action {
    println(s"Processing sentence: $text")
    println()

    val document = processor.annotate(text)
    val mentions = extractorEngine.extractFrom(document).sortBy(_.arguments.size)

    println(s"Tokenized sentence: ${document.sentences.head.getSentenceText}")
    println()
    println("Mention texts:")
    mentions.foreach(mention => println(mention.text))
    println()
    println("Mention details:")
    // sortedEidosMentions.foreach(eidosMention => DisplayUtils.displayEidosMention(eidosMention))
    println()

    val json = webSerializer.processDocument(text, document, mentions)

    Ok(json)
  }
}
