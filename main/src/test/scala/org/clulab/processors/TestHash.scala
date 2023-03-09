package org.clulab.processors

import org.clulab.odin.{CrossSentenceMention, EventMention, ExtractorEngine, Mention, RelationMention, TextBoundMention}
import org.clulab.odin.serialization.json._
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.utils.FileUtils
import org.clulab.utils.Test

import java.io.File

class TestHash extends Test {
  val resourceDir: File = new File("./src/main/resources")
  val customLexiconNer = {
    val kbsAndCaseInsensitiveMatchings: Seq[(String, Boolean)] = Seq(
      ("org/clulab/odinstarter/FOOD.tsv", true)
    )
    val kbs = kbsAndCaseInsensitiveMatchings.map(_._1)
    val caseInsensitiveMatchings = kbsAndCaseInsensitiveMatchings.map(_._2)

    LexiconNER(kbs, caseInsensitiveMatchings, Some(resourceDir))
  }
  val processor = new CluProcessor(optionalNER = Some(customLexiconNer))
  val extractorEngine = {
    val masterResource = "/org/clulab/odinstarter/main.yml"
    val masterFile = new File(resourceDir, masterResource.drop(1))
    val rules = FileUtils.getTextFromFile(masterFile)
    ExtractorEngine(rules, ruleDir = Some(resourceDir))
  }
  val document = processor.annotate("John eats cake.")
  val mentions = extractorEngine.extractFrom(document).sortBy(_.arguments.size)
  val sortedMentions = mentions.sortBy { mention => (mention.startOffset, mention.endOffset) }
  val eventMention = sortedMentions.find(_.isInstanceOf[EventMention]).get.asInstanceOf[EventMention]
  val otherMentions = sortedMentions.filterNot(_.eq(eventMention))
  val relationMention = eventMention.toRelationMention
  val crossSentenceMention = newCrossSentenceMention(eventMention, otherMentions.head, otherMentions.last)
  val allMentions = sortedMentions :+ relationMention :+ crossSentenceMention

  behavior of "Hash"

  it should "compute the expected equivalence hash for a Document" in {
    val expectedHash = -1960515414
    val actualHash = document.equivalenceHash

    actualHash should be (expectedHash)
  }

  def getEquivalenceHash(mention: Mention): Int = mention match {
    case mention: TextBoundMention     => mention.equivalenceHash
    case mention: EventMention         => mention.equivalenceHash
    case mention: RelationMention      => mention.equivalenceHash
    case mention: CrossSentenceMention => mention.equivalenceHash
  }

  def newCrossSentenceMention(mention: EventMention, anchor: Mention, neighbor: Mention): CrossSentenceMention = {
    new CrossSentenceMention(
      mention.labels,
      anchor,
      neighbor,
      mention.arguments,
      mention.document,
      mention.keep,
      mention.foundBy,
      mention.attachments
    )
  }

  it should "compute the expected equivalence hashes for Mentions" in {
    val expectedHashes = Array(-1163474360, 1678747586, 308621545, 1846645205, -1357918569)
    val actualHashes = allMentions.map(getEquivalenceHash)

    actualHashes should be (expectedHashes)
  }

  it should "compute the expected hashCode for Mentions" in {
    val expectedHashes = Array(-681771612, -254169462, -1589508928, 823771056, 1600327181)
    val actualHashes = allMentions.map(_.hashCode)

    actualHashes should be(expectedHashes)
  }
}
