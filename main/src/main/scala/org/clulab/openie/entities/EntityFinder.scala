package org.clulab.openie.entities

import org.clulab.odin.Mention
import org.clulab.processors.Document


trait EntityFinder {

  def extract(doc: Document): Seq[Mention]

}
