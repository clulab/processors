package org.clulab.odin

import org.clulab.odin.impl.{MatchToken, TokenWildcard}
import org.clulab.utils.Test

class TestInst2 extends Test {

  behavior of "MatchToken"

  it should "be distinguishable" in {
    val inst1 = MatchToken(TokenWildcard)
    inst1.posId = 27 // This makes hash codes different.
    val hash1 = inst1.##

    val inst2 = MatchToken(TokenWildcard)
    inst2.posId = 29
    val hash2 = inst2.##

    hash1 should not be (hash2)
    inst1 should not be (inst2)
  }
}
