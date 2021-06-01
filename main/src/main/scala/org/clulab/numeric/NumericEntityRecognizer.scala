package org.clulab.numeric

import org.clulab.odin.ExtractorEngine
import org.clulab.processors.Document
import org.clulab.sequences.LexiconNER
import org.clulab.struct.TrueEntityValidator

class NumericEntityRecognizer {

  // this matches essential dictionaries such as month names
  val lexiconNer = LexiconNER(
    Seq("org/clulab/numeric/MONTH.tsv"),
    Seq(true),
    new TrueEntityValidator,
    useLemmasForMatching = false
  )

  // this matches the grammars for both atomic and compositional entities
  val extractor = {
    val source = io.Source.fromURL(getClass.getResource("/org/clulab/numeric/master.yml"))
    val rules = source.mkString
    source.close()
    ExtractorEngine(rules)
  }

  /** Matches the lexicon NER on this document, setting the `entities` field */
  def matchLexiconNer(document: Document): Unit = {
    for(sent <- document.sentences) {
      val labels = lexiconNer.find(sent)
      sent.entities = Some(labels)
    }
  }

  /**
    * Entry point for numeric entity recognition
    * @param doc Input document
    * @return sets in place the sequence of NER labels and sequence of NER norms (using the TempEval-2 notation)
    */
  def extractFrom(doc:Document): Unit = {
    // dictionaries
    matchLexiconNer(doc)

    // grammars
    val mentions = extractor.extractFrom(doc)
    displayMentions(mentions, doc)

    // set labels and norms
    // TODO
  }
}
