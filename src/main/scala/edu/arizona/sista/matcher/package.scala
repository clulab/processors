package edu.arizona.sista

package object matcher {
  type PostProcessor = Seq[Mention] => Seq[Mention]
  type SentenceToken = (Int, Int)
  type MentionLUT = Map[SentenceToken, Seq[Mention]]
}
