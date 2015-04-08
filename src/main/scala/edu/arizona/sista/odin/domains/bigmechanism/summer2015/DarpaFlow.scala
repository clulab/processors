package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin._

class DarpaFlow(grounder: DarpaFlowStep, coref: DarpaFlowStep)
    extends DarpaFlowStep {

  /** This method goes into odin's ExtractorEngine as the globalAction:
    *
    * {{{
    * val ee = new ExtractorEngine(rules, actions, flow.apply)
    * }}}
    */
  def apply(mentions: Seq[Mention], state: State): Seq[Mention] = {
    // ground mentions
    val groundedMentions = grounder.apply(mentions, state)
    // apply coreference
    val resolvedMentions = coref.apply(groundedMentions, state)
    // success!
    resolvedMentions
  }
}
