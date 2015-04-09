package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin._

trait DarpaFlow {
  /** Gets the candidate mentions and returns the final mentions.
    *
    * @param mentions this iteration's candidate mentions
    * @param state contains mentions from previous iterations
    * @return the final mentions
    */
  def apply(mentions: Seq[Mention], state: State): Seq[Mention]

  /** Composes two instances of DarpaFlow into a single DarpaFlow */
  def andThen(that: DarpaFlow): DarpaFlow = new AndThenDarpaFlow(this, that)
}

class AndThenDarpaFlow(step1: DarpaFlow, step2: DarpaFlow) extends DarpaFlow {
  def apply(mentions: Seq[Mention], state: State): Seq[Mention] =
    step2(step1(mentions, state), state)
}
