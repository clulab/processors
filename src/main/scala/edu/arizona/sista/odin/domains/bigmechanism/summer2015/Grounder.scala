package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin._

class Grounder extends DarpaFlowStep {
  // someday this method will ground mentions
  def apply(mentions: Seq[Mention], state: State): Seq[Mention] = mentions
}
