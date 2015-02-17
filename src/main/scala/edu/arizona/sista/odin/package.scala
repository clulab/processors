package edu.arizona.sista

package object odin {
  type PostProcessor = Seq[Mention] => Seq[Mention]
  type SentenceToken = (Int, Int)
  type MentionLUT = Map[SentenceToken, Seq[Mention]]
}
