package org.clulab.struct

import org.clulab.struct.CorefChains._
import org.clulab.utils.Test

class TestCorefChains extends Test {

  behavior of "CorefChains"

  // This is the former code for regression testing.
  def lessThanForMentions(x: CorefMention, y: CorefMention): Boolean = {
    if (x.sentenceIndex < y.sentenceIndex) return true
    if (x.sentenceIndex > y.sentenceIndex) return false

    if (x.headIndex < y.headIndex) return true
    if (x.headIndex > y.headIndex) return false

    val diffSizeX = x.endOffset - x.startOffset
    val diffSizeY = y.endOffset - y.startOffset
    // These are reversed from the above.
    if (diffSizeX < diffSizeY) return false
    if (diffSizeX > diffSizeY) return true

    true
  }

  it should "sort on sentenceIndex" in {
    val left = CorefMention(0, 1, 0, 0, 0)
    val right = CorefMention(1, 0, 0, 0, 0)

    List(left, right).sorted.head should be theSameInstanceAs (left)
    List(right, left).sorted.head should be theSameInstanceAs (left)

    List(left, right).sortWith(lessThanForMentions).head should be theSameInstanceAs (left)
    List(right, left).sortWith(lessThanForMentions).head should be theSameInstanceAs (left)
  }

  it should "sort on headIndex" in {
    val left = CorefMention(0, 0, 0, 1, 0)
    val right = CorefMention(0, 1, 0, 0, 0)

    List(left, right).sorted.head should be theSameInstanceAs (left)
    List(right, left).sorted.head should be theSameInstanceAs (left)

    List(left, right).sortWith(lessThanForMentions).head should be theSameInstanceAs (left)
    List(right, left).sortWith(lessThanForMentions).head should be theSameInstanceAs (left)
  }

  it should "sort on size" in {
    val left = CorefMention(0, 0, 0, 1, 0)
    val right = CorefMention(0, 0, 0, 0, 0)

    List(left, right).sorted.head should be theSameInstanceAs (left)
    List(right, left).sorted.head should be theSameInstanceAs (left)

    List(left, right).sortWith(lessThanForMentions).head should be theSameInstanceAs (left)
    List(right, left).sortWith(lessThanForMentions).head should be theSameInstanceAs (left)
  }

  it should "sort on index" in {
    val left = CorefMention(0, 0, 0, 1, 0)
    val right = CorefMention(0, 0, 0, 1, 1)

    val leftRight1 = List(left, right).sorted
    val rightLeft1 = List(right, left).sorted

    val leftRight2 = List(left, right).sortWith(lessThanForMentions)
    val rightLeft2 = List(right, left).sortWith(lessThanForMentions)

    leftRight1.head should be theSameInstanceAs (leftRight2.head)
    rightLeft1.head should be theSameInstanceAs (rightLeft2.head)
  }
}
