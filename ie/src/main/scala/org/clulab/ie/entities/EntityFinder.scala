package org.clulab.ie.entities

import org.clulab.odin.Mention
import org.clulab.processors.Document


trait EntityFinder {

  def extract(doc: Document): Seq[Mention]

}
