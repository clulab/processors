package org.clulab.processors.clu.bio

import org.clulab.processors.clu.PreProcessor

/**
  * Preprocesses bio text, including Unicode normalization, and removing figure and table references
  * User: mihais
  * Date: 9/10/17
  */
class BioPreProcessor(removeFigTabReferences:Boolean, removeBibReferences:Boolean) extends PreProcessor {
  override def process(text: String): String = {
    // TODO
  }
}
