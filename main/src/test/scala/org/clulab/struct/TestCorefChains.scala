package org.clulab.struct

import org.clulab.struct.CorefChains._
import org.clulab.utils.Test

class TestCorefChains extends Test {

  behavior of "CorefChains"

  it should "sort on sentenceIndex" in {
    val left = CorefMention(0, 1, 0, 0, 0)
    val right = CorefMention(1, 0, 0, 0, 0)

    List(left, right).sorted.head should be theSameInstanceAs (left)
    List(right, left).sorted.head should be theSameInstanceAs (left)

    List(left, right).sortWith(CorefChains.lessThanForMentions).head should be theSameInstanceAs (left)
    List(right, left).sortWith(CorefChains.lessThanForMentions).head should be theSameInstanceAs (left)
  }

  it should "sort on headIndex" in {
    val left = CorefMention(0, 0, 0, 1, 0)
    val right = CorefMention(0, 1, 0, 0, 0)

    List(left, right).sorted.head should be theSameInstanceAs (left)
    List(right, left).sorted.head should be theSameInstanceAs (left)

    List(left, right).sortWith(CorefChains.lessThanForMentions).head should be theSameInstanceAs (left)
    List(right, left).sortWith(CorefChains.lessThanForMentions).head should be theSameInstanceAs (left)
  }

  it should "sort on size" in {
    val left = CorefMention(0, 0, 0, 1, 0)
    val right = CorefMention(0, 0, 0, 0, 0)

    List(left, right).sorted.head should be theSameInstanceAs (left)
    List(right, left).sorted.head should be theSameInstanceAs (left)

    List(left, right).sortWith(CorefChains.lessThanForMentions).head should be theSameInstanceAs (left)
    List(right, left).sortWith(CorefChains.lessThanForMentions).head should be theSameInstanceAs (left)
  }

  it should "sort on index" in {
    val left = CorefMention(0, 0, 0, 1, 0)
    val right = CorefMention(0, 0, 0, 1, 1)

    val leftRight1 = List(left, right).sorted
    val rightLeft1 = List(right, left).sorted

    val leftRight2 = List(left, right).sortWith(CorefChains.lessThanForMentions)
    val rightLeft2 = List(right, left).sortWith(CorefChains.lessThanForMentions)

    leftRight1.head should be theSameInstanceAs (leftRight2.head)
    rightLeft1.head should be theSameInstanceAs (rightLeft2.head)
  }
}
