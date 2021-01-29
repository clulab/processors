package org.clulab.processors.fastnlp

import org.clulab.dynet.Utils
import org.clulab.processors.Document
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.tokenizer.TokenizerStep
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.struct.GraphMap

/** Adds SRL functionality to FastNLPProcessor */
class FastNLPProcessorWithSemanticRoles(tokenizerPostProcessor:Option[TokenizerStep],
                                        internStrings:Boolean,
                                        withChunks:Boolean,
                                        withRelationExtraction:Boolean,
                                        withDiscourse:Int)
  extends FastNLPProcessor(tokenizerPostProcessor, internStrings, withChunks, withRelationExtraction, withDiscourse) {

  /** Used for SRL */
  lazy val cluProcessor = {
    Utils.initializeDyNet()
    new CluProcessor() {
      // Since this skips CluProcessor.srl() and goes straight for srlSentence(), there isn't
      // a chance to make sure CluProcessor.mtlSrla is initialized, so it is done here.
      assert(this.mtlSrla != null)
    }
  }

  def this(internStrings:Boolean = true,
           withChunks:Boolean = true,
           withRelationExtraction:Boolean = false,
           withDiscourse:Int = ShallowNLPProcessor.NO_DISCOURSE) {
    this(None, internStrings, withChunks, withRelationExtraction, withDiscourse)
  }

  override def srl(doc: Document): Unit = {
    for(sent <- doc.sentences) {
      val words = sent.words

      // The SRL model relies on NEs produced by CluProcessor, so run the NER first
      val (tags, _, preds) = cluProcessor.tagSentence(words)
      val predIndexes = cluProcessor.getPredicateIndexes(preds)
      val entities = cluProcessor.nerSentence(words)
      val semanticRoles = cluProcessor.srlSentence(
        words, tags, entities, predIndexes
      )

      sent.graphs += GraphMap.SEMANTIC_ROLES -> semanticRoles
    }
  }
}
