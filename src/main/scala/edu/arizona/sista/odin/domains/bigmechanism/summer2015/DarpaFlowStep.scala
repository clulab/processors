package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin._

trait DarpaFlowStep {
  /** Gets the candidate mentions and returns the final mentions.
    *
    * @param mentions this iteration's candidate mentions
    * @param state contains mentions from previous iterations
    * @return the final mentions
    */
  def apply(mentions: Seq[Mention], state: State): Seq[Mention]
}
