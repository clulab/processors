package org.clulab.numeric

import org.scalatest.{FlatSpec, Matchers}

class TestNumberParser extends FlatSpec with Matchers {

	"NumberParser" should "parse digits" in {
		val text = "1200"
		val result = NumberParser.parse(text.split(" "))
		result shouldEqual Some(1200)
	}

	it should "fail on invalid input" in {
		val text = "lkjsaflk"
		val result = NumberParser.parse(text.split(" "))
		result shouldEqual None
	}

	it should "fail on empty input" in {
		val result = NumberParser.parse("".split(" "))
		result shouldEqual None
		val result2 = NumberParser.parse(Nil)
		result2 shouldEqual None
	}

	it should "convert words to numbers" in {
		NumberParser.parse("-1".split(" ")) shouldEqual Some(-1)
		NumberParser.parse("1/2".split(" ")) shouldEqual Some(0.5)
		NumberParser.parse("-1/2".split(" ")) shouldEqual Some(-0.5)
		NumberParser.parse("1/-2".split(" ")) shouldEqual Some(-0.5)
		NumberParser.parse("-1/-2".split(" ")) shouldEqual Some(0.5)
		NumberParser.parse("- 1".split(" ")) shouldEqual Some(-1)
		NumberParser.parse("1 / 2".split(" ")) shouldEqual Some(0.5)
		NumberParser.parse("- 1 / 2".split(" ")) shouldEqual Some(-0.5)
		NumberParser.parse("1 / - 2".split(" ")) shouldEqual Some(-0.5)
		NumberParser.parse("- 1 / - 2".split(" ")) shouldEqual Some(0.5)
		NumberParser.parse("1.2".split(" ")) shouldEqual Some(1.2)
		NumberParser.parse("1.2 million".split(" ")) shouldEqual Some(1200000)
		NumberParser.parse("twenty one".split(" ")) shouldEqual Some(21)
		NumberParser.parse("thirty three".split(" ")) shouldEqual Some(33)
		NumberParser.parse("twelve hundred".split(" ")) shouldEqual Some(1200)
		NumberParser.parse("twelve hundred thirty four".split(" ")) shouldEqual Some(1234)
		NumberParser.parse("one thousand two hundred thirty four".split(" ")) shouldEqual Some(1234)
		NumberParser.parse("two thousand one".split(" ")) shouldEqual Some(2001)
		// NumberParser.parse("two thousand and one".split(" ")) shouldEqual Some(2001)
		NumberParser.parse("one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine".split(" ")) shouldEqual Some(123456789)
		NumberParser.parse("5.2 million".split(" ")) shouldEqual Some(5200000)
		NumberParser.parse("5.2 billion".split(" ")) shouldEqual Some(5200000000d)
		NumberParser.parse("5.2 trillion".split(" ")) shouldEqual Some(5200000000000d)
		NumberParser.parse("5.2 quadrillion".split(" ")) shouldEqual Some(5200000000000000d)
		NumberParser.parse("thousand".split(" ")) shouldEqual Some(1000)
		NumberParser.parse("thousands".split(" ")) shouldEqual Some(1000)
		NumberParser.parse("2 thousand".split(" ")) shouldEqual Some(2000)
		NumberParser.parse("2 thousands".split(" ")) shouldEqual Some(2000)
	}

}
