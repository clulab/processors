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
		NumberParser.parse("100 thousand".split(" ")) shouldEqual Some(100000)
		NumberParser.parse("one hundred thousand".split(" ")) shouldEqual Some(100000)
		NumberParser.parse("ONE HUNDRED THOUSAND".split(" ")) shouldEqual Some(100000)
		NumberParser.parse("100,000".split(" ")) shouldEqual Some(100000)
	}

	// New added tests

	it should "parses decimal numbers" in {
		NumberParser.parse("0.3".split(" ")) shouldEqual Some(0.3)
		NumberParser.parse("0.55".split(" ")) shouldEqual Some(0.55)
		NumberParser.parse("0.002".split(" ")) shouldEqual Some(0.002)
	}

	it should "convert numbers and words to numbers" in {
		NumberParser.parse("45.98 thousand".split(" ")) shouldEqual Some(45980)
		NumberParser.parse("0.5 million".split(" ")) shouldEqual Some(500000)
	}

	it should "parse words to numbers" in {
		NumberParser.parse("five hundred million".split(" ")) shouldEqual Some(500000000)

		// TODO: we do not parse "half" and "quarter" now
		//NumberParser.parse("half million".split(" ")) shouldEqual Some(500000)
		//NumberParser.parse("three quarters million".split(" ")) shouldEqual Some(750000)

		// TODO: we do not parse decimals such as "hundredths" now
		//NumberParser.parse("seventy-five hundredths".split(" ")) shouldEqual Some(0.75)
	}

	// ranges are not parsed by the NumberParser, but by a dedicated grammar. TODO: test ranges separately
	/*
	it should "parse numbers given as range" in {
		NumberParser.parse("3-7".split(" ")) shouldEqual "3-7".toString
		NumberParser.parse("6 to 8".split(" ")) shouldEqual "6-8".toString
		NumberParser.parse("from around 400 000 to almost 900 000".split(" ")) shouldEqual "400000--900000".toString
		NumberParser.parse("from 78,000 to 114,000".split(" ")) shouldEqual "78000.0--114000.0".toString
	}
	*/

}
