package org.clulab.processors

import org.clulab.utils.{FileUtils, Sourcer}

class TestLemmatizer extends CluTest {

  behavior of "the lemmatizer"

  it should "not crash when processing this weird file" in {
    val resourceName = "/CORD19_DOC_2762.txt"
    val text = FileUtils.getTextFromResource(resourceName)

    println("Trying to parse file:")
    println(text)

    // if this does not crash we declare success
    val doc = annotate(text)
    doc.sentences should not be empty
  }

  def annotate(text: String): Document = {
    val doc = proc.mkDocument(text)

    proc.annotate(doc)
    doc
  }
}
