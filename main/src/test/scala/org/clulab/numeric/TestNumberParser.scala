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
		NumberParser.parse("twenty one".split(" ")) shouldEqual Some(21)
		NumberParser.parse("thirty three".split(" ")) shouldEqual Some(33)
		NumberParser.parse("twelve hundred".split(" ")) shouldEqual Some(1200)
		NumberParser.parse("two thousand and one".split(" ")) shouldEqual Some(2001)
		NumberParser.parse("two million three hundred forty five thousand six hundred seventy eight".split(" ")) shouldEqual Some(2345678)
	}

}