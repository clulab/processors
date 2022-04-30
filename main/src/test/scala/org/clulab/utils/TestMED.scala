package org.clulab.utils

import org.scalatest.{FlatSpec, Matchers}

class TestMED extends FlatSpec with Matchers {
  behavior of "MED"

  it should "work, substitute = false, transpose = false, capitalize = false" in {
    val allowSubstitute = false
    val allowTranspose = false
    val allowCapitalize = false

    def getDistance(source: String, target: String) = MED(source, target, allowSubstitute, allowTranspose, allowCapitalize).getDistance

    getDistance("cat", "cars") should be (3)
    getDistance("kitten", "sitting") should be (5)
    getDistance("Sunday", "Saturday") should be (4)
    getDistance("meter", "litre") should be (6)
    getDistance("mom", "Mom") should be (2)
  }

  it should "work, substitute = true, transpose = false, capitalize = false" in {
    val allowSubstitute = true
    val allowTranspose = false
    val allowCapitalize = false

    def getDistance(source: String, target: String) = MED(source, target, allowSubstitute, allowTranspose, allowCapitalize).getDistance

    getDistance("cat", "cars") should be (2)
    getDistance("kitten", "sitting") should be (3)
    getDistance("Sunday", "Saturday") should be (3)
    getDistance("meter", "litre") should be (4)
    getDistance("mom", "Mom") should be (1)
  }

  it should "work, substitute = false, transpose = true, capitalize = false" in {
    val allowSubstitute = false
    val allowTranspose = true
    val allowCapitalize = false

    def getDistance(source: String, target: String) = MED(source, target, allowSubstitute, allowTranspose, allowCapitalize).getDistance

    getDistance("cat", "cars") should be (3)
    getDistance("kitten", "sitting") should be (5)
    getDistance("Sunday", "Saturday") should be (4)
    getDistance("meter", "litre") should be (5)
    getDistance("mom", "Mom") should be (2)
  }

  it should "work, substitute = true, transpose = true, capitalize = false" in {
    val allowSubstitute = true
    val allowTranspose = true
    val allowCapitalize = false

    def getDistance(source: String, target: String) = MED(source, target, allowSubstitute, allowTranspose, allowCapitalize).getDistance

    getDistance("cat", "cars") should be (2)
    getDistance("kitten", "sitting") should be (3)
    getDistance("Sunday", "Saturday") should be (3)
    getDistance("meter", "litre") should be (3)
    getDistance("mom", "Mom") should be (1)
  }

  it should "work, substitute = false, transpose = false, capitalize = true" in {
    val allowSubstitute = false
    val allowTranspose = false
    val allowCapitalize = true

    def getDistance(source: String, target: String) = MED(source, target, allowSubstitute, allowTranspose, allowCapitalize).getDistance

    getDistance("cat", "cars") should be (3)
    getDistance("kitten", "sitting") should be (5)
    getDistance("Sunday", "Saturday") should be (4)
    getDistance("meter", "litre") should be (6)
    getDistance("mom", "Mom") should be (0)
  }

  it should "work, substitute = true, transpose = false, capitalize = true" in {
    val allowSubstitute = true
    val allowTranspose = false
    val allowCapitalize = true

    def getDistance(source: String, target: String) = MED(source, target, allowSubstitute, allowTranspose, allowCapitalize).getDistance

    getDistance("cat", "cars") should be (2)
    getDistance("kitten", "sitting") should be (3)
    getDistance("Sunday", "Saturday") should be (3)
    getDistance("meter", "litre") should be (4)
    getDistance("mom", "Mom") should be (0)
  }

  it should "work, substitute = false, transpose = true, capitalize = true" in {
    val allowSubstitute = false
    val allowTranspose = true
    val allowCapitalize = true

    def getDistance(source: String, target: String) = MED(source, target, allowSubstitute, allowTranspose, allowCapitalize).getDistance

    getDistance("cat", "cars") should be (3)
    getDistance("kitten", "sitting") should be (5)
    getDistance("Sunday", "Saturday") should be (4)
    getDistance("meter", "litre") should be (5)
    getDistance("mom", "Mom") should be (0)
  }

  it should "work, substitute = true, transpose = true, capitalize = true" in {
    val allowSubstitute = true
    val allowTranspose = true
    val allowCapitalize = true

    def getDistance(source: String, target: String) = MED(source, target, allowSubstitute, allowTranspose, allowCapitalize).getDistance

    getDistance("cat", "cars") should be (2)
    getDistance("kitten", "sitting") should be (3)
    getDistance("Sunday", "Saturday") should be (3)
    getDistance("meter", "litre") should be (3)
    getDistance("mom", "Mom") should be (0)
  }
}
