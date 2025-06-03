package org.clulab.processors.webapp2

import com.typesafe.config.{Config, ConfigBeanFactory}
import org.clulab.odin.{CrossSentenceMention, EventMention, ExtractorEngine, Mention, RelationMention, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.processors.clu.BalaurProcessor
import org.clulab.processors.webapp2.serialization.WebSerializer
import org.clulab.sequences.LexiconNER
import org.clulab.utils.{FileUtils, Unordered}
import org.clulab.utils.Unordered.OrderingOrElseBy
import org.json4s.jackson.JsonMethods

import java.io.PrintStream
import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._

class InformationExtractor(config: Config, printStream: PrintStream) {
  val customLexiconNerConfigs = config.getConfigList("customLexiconNer").asScala.map { config =>
    ConfigBeanFactory.create(config, classOf[CustomLexiconNerConfig])
  }
  val extractorEngineConfig = ConfigBeanFactory.create(config.getConfig("extractorEngine"), classOf[ExtractorEngineConfig])

  val processor = {
    val kbs = customLexiconNerConfigs.map(_.kb)
    val caseInsensitiveMatchings = customLexiconNerConfigs.map(_.caseInsensitiveMatching)
    val customLexiconNer = LexiconNER(kbs, caseInsensitiveMatchings, None)
    val processor = new BalaurProcessor(lexiconNerOpt = Some(customLexiconNer))

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

    def toRow(field: String, text: String): Unit = printStream.println(s"$indent$field: $text")

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
    printStream.println()
  }

  val webSerializer = new WebSerializer()

  def annotate(text: String): Document = {
    val document = processor.annotate(text)


    document
  }

  def extract(document: Document): Seq[Mention] = {
    val mentions = extractorEngine.extractFrom(document).sorted

    mentions
  }

  def processDocument(text: String, document: Document, mentions: Seq[Mention]): String = {
    val jValue = webSerializer.processDocument(text, document, mentions)
    val json = JsonMethods.pretty(jValue)

    json
  }
}


case class CustomLexiconNerConfig(@BeanProperty var kb: String, @BeanProperty var caseInsensitiveMatching: Boolean) {
  def this() = this("", false)
}

case class ExtractorEngineConfig(@BeanProperty var rules: String) {
  def this() = this("")
}
