package org.clulab.odin

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import org.clulab.odin.impl.RuleReader
import org.clulab.utils.FileUtils
import org.scalatest.{FlatSpec, Matchers}

class TestMarkdownGeneration extends FlatSpec with Matchers {

  behavior of "MarkdownGeneration"

  it should "not crash" in {

    val tempDir = Files.createTempDirectory("temp_test")
    val rr = new RuleReader(new Actions, StandardCharsets.UTF_8)

    val master = FileUtils.getTextFromResource("/org/clulab/odin/grammars/embeddings.yml")
    noException shouldBe thrownBy{
      rr.exportExtractionSchemas(master, s"$tempDir/extractions.md")
    }
    noException shouldBe thrownBy{
      rr.exportRuleSchemas(master, s"$tempDir/rules.md")
    }

  }

  it should "properly aggregate argument requiredness" in {
    val rules =
      """
        |rules:
        | - name: Rule1
        |   label: TestMention
        |   keep: true
        |   pattern: |
        |     trigger = eat
        |     agent: Entity = >nsubj []
        |     theme: Entity = >dobj []
        |     alwaysOpt: Entity? = >abc []
        |
        | - name: Rule2
        |   label: TestMention
        |   keep: true
        |   pattern: |
        |     trigger = eat
        |     agent: Entity? = >nsubj []
        |     theme: Entity = >dobj []
        |     alwaysOpt: Entity? = >abc []
        |
        | - name: Rule3
        |   label: TestMention
        |   keep: true
        |   pattern: |
        |     trigger = eat
        |     agent: Entity? = >nsubj []
        |     theme: Entity = >dobj []
        |     other: Entity = >dobj >amod []
        |     alwaysOpt: Entity? = >abc []
        |""".stripMargin

    val rr = new RuleReader(new Actions, StandardCharsets.UTF_8)
    val extractionsSchemas = rr.extractionSchemaObjects(rules)
    extractionsSchemas should have length (1)

    val schema = extractionsSchemas.head
    val aggregated = schema.aggregateArgs()
    // agent
    val (aLabs, aQuants, aReqs) = aggregated("agent")
    aLabs should include("Entity")
    aQuants should include("?")
    aQuants should include("_none_")
    aReqs should include("true")
    aReqs should include("false")
    val (tLabs, tQuants, tReqs) = aggregated("theme")
    tLabs should include("Entity")
    tQuants should include("_none_")
    tQuants should not include("?")
    tReqs should include("true")
    tReqs should not include("false")
    val (oLabs, oQuants, oReqs) = aggregated("other")
    oLabs should include("Entity")
    oQuants should include("_none_")
    oQuants should not include("?")
    oReqs should include("true")
    oReqs should include("false")
    val (aoLabs, aoQuants, aoReqs) = aggregated("alwaysOpt")
    aoLabs should include("Entity")
    aoQuants should include("?")
    aoQuants should not include("_none_")
    aoReqs should not include("true")
    aoReqs should include("false")

  }
}
