package edu.arizona.sista.processor.fastnlp

import edu.arizona.sista.processor.corenlp.CoreNLPProcessor
import edu.arizona.sista.processor.Document

/**
 * Fast NLP tools
 * Uses most of CoreNLP but replaces its parser with maltparser
 * This means that constituent trees and coreference, which depends on that, are not available
 * User: mihais
 * Date: 1/4/14
 */
class FastNLPProcessor(internStrings:Boolean = true) extends CoreNLPProcessor(internStrings) {
  override def parse(doc:Document) {

  }

  override def resolveCoreference(doc:Document) {
    // FastNLP does not offer coreference resolution yet
  }
}
