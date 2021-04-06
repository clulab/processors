package org.clulab.timex

import org.clulab.dynet.Utils
import org.clulab.odin.{ExtractorEngine, Mention, TextBoundMention}
import org.clulab.processors.{Document, Processor}
import org.clulab.processors.clu.CluProcessor

class TimexExtractor {
  val rules = mkRules()

  val engine = ExtractorEngine(rules)

  def mkRules(): String = {
    val source = io.Source.fromURL(getClass.getResource("/org/clulab/timex/master.yml"))
    val rules = source.mkString
    source.close()
    rules
  }

  def annotate(doc: Document): Seq[Mention] = {
    val mentions = engine.extractFrom(doc).sortBy(m => (m.sentence, m.getClass.getSimpleName))
    mentions
  }
}

object TimexExtractor {
  def main(args: Array[String]): Unit = {
    Utils.initializeDyNet()

    val sampleText = "They got married on February 2000."
    val extractor = new TimexExtractor
    val proc = new CluProcessor()

    val doc = shallowAnnotate(proc, sampleText)
    val timexMentions = extractor.annotate(doc)

    displayMentions(timexMentions, doc)
  }

  /** Shallow annotations including only tokenization and POS tagging */
  def shallowAnnotate(proc: Processor, text: String): Document = {
    val doc = proc.mkDocument(text)
    proc.tagPartsOfSpeech(doc)
    doc
  }

  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText)
      println("Tokens: " + (s.words.indices, s.words, s.tags.get).zipped.mkString(", "))
      println

      val sortedMentions = mentionsBySentence(i).sortBy(_.label)
      println("entities:")
      sortedMentions foreach displayMention

      println("=" * 50)
    }
  }

  def displayMention(mention: Mention) {
    val boundary = s"\t${"-" * 30}"
    println(s"${mention.labels} => ${mention.text}")
    println(boundary)
    println(s"\tRule => ${mention.foundBy}")
    val mentionType = mention.getClass.toString.split("""\.""").last
    println(s"\tType => $mentionType")
    println(boundary)
    mention match {
      case tb: TextBoundMention =>
        println(s"\t${tb.labels.mkString(", ")} => ${tb.text}")
      case _ => ()
    }
    println(s"$boundary\n")
  }
}