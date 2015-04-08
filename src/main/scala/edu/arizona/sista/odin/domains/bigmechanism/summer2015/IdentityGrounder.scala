package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin._

class IdentityGrounder extends DarpaFlowStep {
  // A NOP: returns all mentions unchanged
  def apply(mentions: Seq[Mention], state: State): Seq[Mention] = mentions
}
