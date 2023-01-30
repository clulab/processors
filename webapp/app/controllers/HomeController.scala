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
      .orElseBy(_.sentence)
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

    def toRow(field: String, text: String): String = s"$indent$field: $text"

    def toRows(field: String, texts: Seq[String]): String = toRow(field, texts.mkString(" "))

    toRow ("       Name", name)
    toRow ("       Type", mention.getClass.getSimpleName)
    toRow ("    FoundBy", mention.foundBy)
    toRow ("   Sentence", mention.sentenceObj.getSentenceText)
    toRows("     Labels", labels)
    toRows("      Words", words)
    toRows("       Tags", tags)
    toRows("     Lemmas", lemmas)
    toRows("   Entities", entities)
    toRows("      Norms", norms)
    toRows("     Chunks", chunks)
    toRows("        Raw", raws)
    toRows("Attachments", mention.attachments.toSeq.map(_.toString).sorted)

    mention match {
      case textBoundMention: TextBoundMention =>
      case eventMention: EventMention =>
        toRow("    Trigger", "")
        printMention(eventMention.trigger, None, depth + 1)
      case relationMention: RelationMention =>
      case crossSentenceMention: CrossSentenceMention =>
      case _ =>
    }

    if (mention.arguments.nonEmpty) {
      toRow("  Arguments", "")
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
    document.sentences.foreach { sentence =>
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
