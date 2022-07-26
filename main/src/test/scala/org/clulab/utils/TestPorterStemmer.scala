package org.clulab.utils

import StringUtils.porterStem
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestPorterStemmer extends AnyFlatSpec with Matchers {
  "PorterStemmer" should "stem words properly" in {
    porterStem("houses") should be ("hous")
    porterStem("cement") should be ("cement")
    porterStem("adjustement") should be ("adjust")
    porterStem("babies") should be ("babi")
  }
}
