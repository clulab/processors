package edu.arizona.sista

package object odin {
  type GlobalAction = (Seq[Mention], State) => Seq[Mention]
  type SentenceToken = (Int, Int)
  type MentionLUT = Map[SentenceToken, Seq[Mention]]
}
