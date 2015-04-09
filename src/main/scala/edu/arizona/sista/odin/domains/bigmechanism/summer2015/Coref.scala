package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin._

class Coref extends DarpaFlow {
  // someday this method will do coreference resolution
  def apply(mentions: Seq[Mention], state: State): Seq[Mention] = mentions
}
