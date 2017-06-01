package org.clulab.odin

import java.io._
import java.nio.charset.{ StandardCharsets, MalformedInputException }
import org.clulab.odin._
import org.scalatest._

class TestExtractorEngine extends FlatSpec with Matchers {

  "ExtractorEngine" should "read utf8 encoded data" in {
    var stream = getClass.getResourceAsStream("/org/clulab/odin/master_utf8.yml")
    lazy val engine = ExtractorEngine.fromStream(stream, new Actions, identityAction, StandardCharsets.UTF_8)
    noException should be thrownBy engine
    engine.extractors.head.name should equal ("lingüística")
  }

  it should "read latin1 encoded data" in {
    val stream = getClass.getResourceAsStream("/org/clulab/odin/master_latin1.yml")
    lazy val engine = ExtractorEngine.fromStream(stream, new Actions, identityAction, StandardCharsets.ISO_8859_1)
    noException should be thrownBy engine
    engine.extractors.head.name should equal ("lingüística")
  }

  it should "fail with an exception when byte sequence is not valid for given charset" in {
    var stream = getClass.getResourceAsStream("/org/clulab/odin/master_latin1.yml")
    lazy val engine = ExtractorEngine.fromStream(stream, new Actions, identityAction, StandardCharsets.UTF_8)
    a [MalformedInputException] should be thrownBy engine
  }

  it should "read data incorrectly if using the wrong charset" in {
    val stream = getClass.getResourceAsStream("/org/clulab/odin/master_utf8.yml")
    val engine = ExtractorEngine.fromStream(stream, new Actions, identityAction, StandardCharsets.ISO_8859_1)
    engine.extractors.head.name should not equal ("lingüística")
  }


}
