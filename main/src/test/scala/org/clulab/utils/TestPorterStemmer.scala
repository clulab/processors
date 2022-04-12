package org.clulab.utils

import org.scalatest.{FlatSpec, Matchers}
import StringUtils.porterStem

class TestPorterStemmer extends FlatSpec with Matchers {
  "PorterStemmer" should "stem words properly" in {
    porterStem("houses") should be ("hous")
    porterStem("cement") should be ("cement")
    porterStem("adjustement") should be ("adjust")
    porterStem("babies") should be ("babi")
  }
}
