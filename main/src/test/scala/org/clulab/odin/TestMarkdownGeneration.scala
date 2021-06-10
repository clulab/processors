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
      rr.exportExtractionSchemasFromMaster(master, s"$tempDir/extractions.md")
    }
    noException shouldBe thrownBy{
      rr.exportRuleSchemasFromMaster(master, s"$tempDir/rules.md")
    }

  }

}
