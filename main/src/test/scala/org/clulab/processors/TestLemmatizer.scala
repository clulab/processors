package org.clulab.processors

import org.clulab.utils.Sourcer

class TestLemmatizer extends FatdynetTest {

  "the lemmatizer" should "not crash when processing this weird file" in {
    val source = Sourcer.sourceFromResource("/CORD19_DOC_2762.txt")
    val sb = new StringBuilder
    for(line <- source.getLines()) {
      sb.append(line)
      sb.append("\n")
    }
    source.close()

    val text = sb.toString()
    println("Trying to parse file:")
    println(text)

    // if this does not crash we declare success
    val doc = annotate(text)
    doc.sentences.length > 0 should be (true)
  }

  def annotate(text:String): Document = {
    val doc = proc.mkDocument(text)
    proc.mkConstEmbeddings(doc)
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    doc
  }
}
