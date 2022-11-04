package org.clulab.odin

import org.clulab.odin.impl.{Done, MatchToken, Pass, RegexStringMatcher, Split, TokenWildcard, WordConstraint}
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

  it should "distinguish complicated instances" in {
    val inst0 = {
      val inst = MatchToken(TokenWildcard)
      inst.next = {
        val inst = Split(
          {
            val inst = MatchToken(TokenWildcard)
            inst.next = {
              val inst = MatchToken(new WordConstraint(new RegexStringMatcher("Accepted|^FAO$".r)))
              inst.next = Done
              inst
            }
            inst
          },
          {
            val inst = new Pass
            inst.next = {
              val inst = MatchToken(new WordConstraint(new RegexStringMatcher("Accepted|^FAO$".r)))
              inst.next = Done
              inst
            }
            inst
          }
        )
        inst
      }
      inst
    }
    val inst1 = {
      val inst = MatchToken(TokenWildcard)
      inst.next = {
        val inst = MatchToken(new WordConstraint(new RegexStringMatcher("Accepted|^FAO$".r)))
        inst.next = Done
        inst
      }
      inst
    }

    // These are the same.
    val inst0Hash = inst0.##
    val inst1Hash = inst1.##

    // These are different.
    val inst0HashNext = inst0.next.##
    val inst1HashNext = inst1.next.##

    val equiv = inst0 == inst1 // true
    val equivNext = inst0.next == inst1.next // false
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

}
