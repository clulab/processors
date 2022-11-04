package org.clulab.odin

import org.clulab.odin.impl.{Constant, Done, ExactStringMatcher, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, RegexStringMatcher, SaveEnd, SaveStart, Split, TokenWildcard, WordConstraint}
import org.clulab.utils.Test

class TestInst extends Test {

  behavior of "Inst"

  it should "not distinguish Done and Done" in {
    val done1 = Done
    val done2 = Done

    val done1Hash = done1.##
    val done2Hash = done2.##

    done1 should be (done2)
  }

  it should "distinguish Done and Pass" in {
    val done = Done
    val pass = Pass

    val doneHash = done.##
    val passHash = pass.##

    done should not be (pass)
  }

  it should "distinguish Done and MatchToken" in {
    val done = Done
    val matchToken = MatchToken(TokenWildcard)

    done should not be (matchToken)
  }

  //  Inst - trait
  //  Done - this
  //  Pass - copy
  //  Split - deep
  //  SaveStart - copy
  //  SaveEnd - copy
  //  MatchToken - copy
  //  MatchMention - copy
  //  MatchSentenceStart - copy
  //  MatchSentenceEnd - copy
  //  MatchLookAhead - deep
  //  MatchLookBehind - deep

  {
    // true is for deep, false is for shallow
    val instAndDeeps = Seq(
      // (Done, false), // This won't work because it is an object!
      (Pass(), false),
      (Split(MatchSentenceStart(), MatchSentenceEnd()), true),
      (SaveStart("hello"), false),
      (SaveEnd("goodbye"), false),
      (MatchToken(TokenWildcard), false),
      (MatchMention(new ExactStringMatcher("string"), None, None), false),
      (MatchSentenceStart(), false),
      (MatchSentenceEnd(), false),
      (MatchLookAhead(Pass(), true), true),
      (MatchLookBehind(Pass(), true), true)
    )

    instAndDeeps.foreach { case (inst, deep) =>
      it should s"copy ${inst.getClass.getSimpleName} appropriately" in {
        {
          val duplicate = inst.dup()

          duplicate.nextOpt should be (empty)
        }
        {
          val deepcopy = inst.deepcopy()

          deepcopy.nextOpt should be (empty)
        }
        {
          inst.nextOpt = Option(Pass())
          val duplicate = inst.dup()

          duplicate.nextOpt should be (empty)
        }
        {
          val deepcopy = inst.deepcopy()

          deepcopy.nextOpt should be (defined)
        }
      }
    }
  }

  it should "distinguish complicated instances" in {
    val inst0 = {
      val inst = MatchToken(TokenWildcard)
      inst.nextOpt = {
        val inst = Split(
          {
            val inst = MatchToken(TokenWildcard)
            inst.nextOpt = {
              val inst = MatchToken(new WordConstraint(new RegexStringMatcher("Accepted|^FAO$".r)))
              inst.nextOpt = Option(Done)
              Option(inst)
            }
            inst
          },
          {
            val inst = new Pass
            inst.nextOpt = {
              val inst = MatchToken(new WordConstraint(new RegexStringMatcher("Accepted|^FAO$".r)))
              inst.nextOpt = Option(Done)
              Option(inst)
            }
            inst
          }
        )
        Option(inst)
      }
      inst
    }
    val inst1 = {
      val inst = MatchToken(TokenWildcard)
      inst.nextOpt = {
        val inst = MatchToken(new WordConstraint(new RegexStringMatcher("Accepted|^FAO$".r)))
        inst.nextOpt = Option(Done)
        Option(inst)
      }
      inst
    }

    // These are the same.
    val inst0Hash = inst0.##
    val inst1Hash = inst1.##

    // These are different.
    val inst0HashNext = inst0.nextOpt.##
    val inst1HashNext = inst1.nextOpt.##

    val equiv = inst0 == inst1 // true
    val equivNext = inst0.nextOpt == inst1.nextOpt // false
    inst0 should not be (inst1)
  }

  case class CaseClass(value: Int) {
    var other: Int = 0
  }

  behavior of "CaseClasses"

  it should "compare as intended" in {
    val inst1 = CaseClass(42)
    val inst2 = CaseClass(42)
    inst2.other = inst1.other + 1

    inst1 should be (inst2)
  }

  behavior of "tuple hashes"

  it should "be called recursively" in {

    case class MyTuple(one: String, two: String) {
      var hashed = false
      override def hashCode: Int = {
        hashed = true
        5
      }
    }

    val myTuple = MyTuple("one", "two")
    val tuple = ("one", "two", myTuple).##

    myTuple.hashed should be (true)
  }
}
