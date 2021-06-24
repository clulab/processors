package org.clulab.numeric

import org.clulab.numeric.actions.NumericActions
import org.clulab.odin.{ExtractorEngine, Mention}
import org.clulab.processors.Document
import org.clulab.sequences.LexiconNER
import org.clulab.struct.TrueEntityValidator

import scala.collection.mutable.ArrayBuffer

class NumericEntityRecognizer {

  // this matches essential dictionaries such as month names
  val lexiconNer = LexiconNER(
    Seq(
      "org/clulab/numeric/MONTH.tsv",
      "org/clulab/numeric/MEASUREMENT-UNIT.tsv"
    ),
    Seq(
      false, // false = case sensitive matching
      true
    ),
    new TrueEntityValidator,
    useLemmasForMatching = false
  )

  val actions = new NumericActions

  // this matches the grammars for both atomic and compositional entities
  val extractor = {
    val source = io.Source.fromURL(getClass.getResource("/org/clulab/numeric/master.yml"))
    val rules = source.mkString
    source.close()
    ExtractorEngine(rules, actions, actions.cleanupAction)
  }

  /** Matches the lexicon NER on this document, setting the `entities` field */
  def matchLexiconNer(document: Document): Seq[Option[Array[String]]] = {
    val originalEntities = new ArrayBuffer[Option[Array[String]]]()

    for(sent <- document.sentences) {
      originalEntities += sent.entities

      val labels = lexiconNer.find(sent)
      // this needs to happen in place, otherwise Odin does not see these labels
      // we will restore the original Sentence.entities at the end in `extractFrom`
      sent.entities = Some(labels)
    }

    originalEntities
  }

  /**
    * Entry point for numeric entity recognition
    * @param doc Input document
    * @return sets in place the sequence of NER labels and sequence of NER norms (using the TempEval-2 notation)
    */
  def extractFrom(doc:Document): Seq[Mention] = {
    // dictionaries
    val originalEntities = matchLexiconNer(doc)

    // grammars
    var mentions = extractor.extractFrom(doc)

    // restore the original entities
    for(i <- originalEntities.indices) {
      doc.sentences(i).entities = originalEntities(i)
    }

    // global actions *after* all grammars are done
    mentions = actions.cleanupAction(mentions)

    mentions
  }
}
