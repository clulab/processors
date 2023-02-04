package org.clulab.processors.webapp.controllers

import org.clulab.odin.{CrossSentenceMention, EventMention, ExtractorEngine, Mention, RelationMention, TextBoundMention}
import org.clulab.processors.Processor
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.webapp.serialization.WebSerializer
import org.clulab.sequences.LexiconNER
import org.clulab.utils.{FileUtils, Unordered}
import org.clulab.utils.Unordered.OrderingOrElseBy
import com.typesafe.config.{ConfigBeanFactory, ConfigFactory}
import play.api.mvc._
import play.api.mvc.Action

import javax.inject._
import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._
import scala.util.Try

@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def initialize(): (Processor, ExtractorEngine) = {
    println("[processors] Initializing the processor ...")

    val config = ConfigFactory.load("application")
        .withFallback(ConfigFactory.load("processors"))
    val customLexiconNerConfigs = config.getConfigList("customLexiconNer").asScala.map { config =>
      ConfigBeanFactory.create(config, classOf[CustomLexiconNerConfig])
    }
    val extractorEngineConfig = ConfigBeanFactory.create(config.getConfig("extractorEngine"), classOf[ExtractorEngineConfig])

    val processor = {
      val kbs = customLexiconNerConfigs.map(_.kb)
      val caseInsensitiveMatchings = customLexiconNerConfigs.map(_.caseInsensitiveMatching)
      val customLexiconNer = LexiconNER(kbs, caseInsensitiveMatchings, None)
      val processor = new CluProcessor(optionalNER = Some(customLexiconNer))

      processor
    }
    val extractorEngine: ExtractorEngine = {
      val rules = FileUtils.getTextFromResource(extractorEngineConfig.rules)
      val extractorEngine = ExtractorEngine(rules)

      extractorEngine
    }

    {
      val document = processor.annotate("John eats cake.")
      extractorEngine.extractFrom(document)
    }
    println("[processors] Completed Initialization ...")
    (processor, extractorEngine)
  }

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

    def toRow(field: String, text: String): Unit = println(s"$indent$field: $text")

    def toRows(field: String, texts: Seq[String]): Unit = toRow(field, texts.mkString(" "))

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

  val webSerializer = new WebSerializer()
  val (processor, extractorEngine) = initialize()

  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def parseText(text: String): Action[AnyContent] = Action {
    println("Text:")
    println(text)
    println()

    val document = processor.annotate(text)

    println("Sentences:")
    document.sentences.foreach { sentence =>
      println(sentence.getSentenceText)
    }
    println()

    val mentions = extractorEngine.extractFrom(document).sorted

    println("Mentions:")
    mentions.foreach { mention =>
      printMention(mention)
    }
    println()

    val json = webSerializer.processDocument(text, document, mentions)

    Ok(json)
  }
}

case class CustomLexiconNerConfig(@BeanProperty var kb: String, @BeanProperty var caseInsensitiveMatching: Boolean) {
  def this() = this("", false)
}

case class ExtractorEngineConfig(@BeanProperty var rules: String) {
  def this() = this("")
}
