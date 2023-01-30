package controllers

import org.clulab.odin.{CrossSentenceMention, EventMention, ExtractorEngine, Mention, RelationMention, TextBoundMention}
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.{LexiconNER, MemoryStandardKbSource, NoLexicalVariations}
import org.clulab.struct.TrueEntityValidator
import org.clulab.utils.Unordered
import org.clulab.utils.Unordered.OrderingOrElseBy
import play.api.mvc._
import play.api.mvc.Action

import javax.inject._

@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
  implicit val mentionOrder = {
    val mentionRank: Map[Class[_], Int] = Map(
      classOf[TextBoundMention] -> 0,
      classOf[EventMention] -> 1,
      classOf[RelationMention] -> 2,
      classOf[CrossSentenceMention] -> 3
    )

    Unordered[Mention]
      .orElseBy { mention => mentionRank.getOrElse(mention.getClass, mentionRank.size) }
      .orElseBy(_.getClass.getName)
      .orElseBy(_.arguments.size)
      .orElseBy(_.tokenInterval)
      .orElse(-1)
  }

  println("[processors] Initializing the processor ...")

  val webSerializer = new WebSerializer()
  val foodKbSource = {
    val lines = Array(
      "cake",
      "pizza",
      "burger",
      "pain au chocolat"
    )
    new MemoryStandardKbSource("FOOD", lines, caseInsensitiveMatching = true)
  }
  val customLexiconNer = LexiconNER(Seq(foodKbSource), None, new NoLexicalVariations, new TrueEntityValidator,
      useLemmasForMatching = false, defaultCaseInsensitive = false)
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
    |  - name: people-eat-food
    |    priority: "2"
    |    label: Eating
    |    example: "John eats cake"
    |    graph: "hybrid"
    |    pattern: |
    |      trigger = [lemma=/eat/ & tag=/^V/]
    |      food:Food = dobj
    |      person:Person = nsubj
    |
    |""".stripMargin
  val extractorEngine: ExtractorEngine = ExtractorEngine(rules)

  {
    val document = processor.annotate("John eats cake.")
    extractorEngine.extractFrom(document)
  }
  println("[processors] Completed Initialization ...")

  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def printMention(mention: Mention, nameOpt: Option[String] = None, depth: Int = 0): Unit = {
    val sentence = mention.sentenceObj
    val tokenInterval = mention.tokenInterval
    val indent = "    " * depth
    val name = nameOpt.getOrElse("<none>")
    val labels = mention.labels
    val words = tokenInterval.map(sentence.words)
    val tags = sentence.tags.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val lemmas = sentence.lemmas.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val entities = sentence.entities.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val norms = sentence.norms.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val chunks = sentence.chunks.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val raws = tokenInterval.map(sentence.raw)

    println(indent + "       Name: " + name)
    println(indent + "       Type: " + mention.getClass.getSimpleName)
    println(indent + "    FoundBy: " + mention.foundBy)
    println(indent + "   Sentence: " + mention.sentenceObj.getSentenceText)
    println(indent + "     Labels: " + labels.mkString(" "))
    println(indent + "      Words: " + words.mkString(" "))
    println(indent + "       Tags: " + tags.mkString(" "))
    println(indent + "     Lemmas: " + lemmas.mkString(" "))
    println(indent + "   Entities: " + entities.mkString(" "))
    println(indent + "      Norms: " + norms.mkString(" "))
    println(indent + "     Chunks: " + chunks.mkString(" "))
    println(indent + "        Raw: " + raws.mkString(" "))
    println(indent + "Attachments: " + mention.attachments.map(_.toString).mkString(" "))

    mention match {
      case textBoundMention: TextBoundMention =>
      case eventMention: EventMention =>
        println(indent + "    Trigger:")
        printMention(eventMention.trigger, None, depth + 1)
      case relationMention: RelationMention =>
      case crossSentenceMention: CrossSentenceMention =>
      case _ =>
    }

    if (mention.arguments.nonEmpty) {
      println(indent + "  Arguments:")
      for (name <- mention.arguments.keys.toSeq.sorted; mention <- mention.arguments(name).sorted)
        printMention(mention, Some(name), depth + 1)
    }
    println()
  }

  def parseText(text: String): Action[AnyContent] = Action {
    println("Processing text:")
    println(text)
    println()

    val document = processor.annotate(text)
    val mentions = extractorEngine.extractFrom(document).sorted

    println("Tokenized sentences:")
    document.sentences.zipWithIndex.foreach { case (sentence, index) =>
      println(sentence.getSentenceText)
    }
    println()

    println("Mentions:")
    mentions.foreach { mention =>
      printMention(mention)
    }
    println()

    val json = webSerializer.processDocument(text, document, mentions)

    Ok(json)
  }
}
