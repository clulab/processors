package edu.arizona.sista

package object odin {
  type Action = (Seq[Mention], State) => Seq[Mention]
  type SentenceToken = (Int, Int)
  type MentionLUT = Map[SentenceToken, Seq[Mention]]

  type SynPath = Seq[(Int, Int, String)] // governor, dependent, label

  def identityAction(mentions: Seq[Mention], state: State): Seq[Mention] = mentions
}
