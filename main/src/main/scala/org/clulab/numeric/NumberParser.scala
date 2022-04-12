package org.clulab.numeric

import scala.collection.mutable.ArrayBuffer

/**
  * Parses textual numbers, e.g., "twelve hundred", into numbers, e.g., "1200"
  */
object NumberParser {
  val numWithOrdinalSuffix = """^\d+(st|nd|rd|th)$""".r

  def parse(words: Seq[String]): Option[Double] = {
    words match {
      case Seq() =>
        None
      case words =>
        val cleanWords = removePlusMinus(words).flatMap { w =>
          // lowercase
          var word = w.toLowerCase()
          // remove commas from numbers like 100,000
          word = word.replace(",", "")
          // remove "+"
          word = word.replace("+", "")
          // remove 's' from words like "thousands"
          if (word.endsWith("s")) {
            word = word.dropRight(1)
          }
          // remove ordinal sufixes from numbers, e.g., "st" from "1st"
          if (numWithOrdinalSuffix.findFirstIn(word).nonEmpty) {
              word = word.dropRight(2)
          }
          // split on dashes
          if (hyphenated.contains(word)) {
              word.split("-")
          } else {
              Array(word)
          }
        }.filterNot(_.isEmpty)
        if(cleanWords.nonEmpty) {
          parseWords(cleanWords) orElse parseNumeric(cleanWords)
        } else {
          None
        }
    }
  }

  /** Remove the part of the number including and following +/-, e.g., in "45 +/- 13", we remove "+/- 13" */
  // Unicode \u00b1 seems to be already converted to the trigraph +/-.
  def removePlusMinus(words: Seq[String]): Seq[String] = words.takeWhile(_ != "+/-")

  def parseNumeric(words: Seq[String]): Option[Double] = {
    try {
      var number: Double = 1
      var numerator: Option[Double] = None
      for (w <- words) {
        w match {
          case "-" =>
            number *= -1
          case "/" =>
            numerator = Some(number)
            number = 1
          case w if w.contains("/") =>
            val Array(w1, w2) = w.split("/")
            number *= w1.toDouble
            numerator = Some(number)
            number = w2.toDouble
          case w if americanNumberSystem.contains(w) =>
            number *= americanNumberSystem(w)
          case w =>
            number *= w.toDouble
        }
      }
      numerator match {
        case None => Some(number)
        case Some(n) => Some(n / number)
      }
    } catch {
      case _: Exception => None
    }
  }

  def parseWords(words: Seq[String]): Option[Double] = {
    // if single token then return corresponding number
    if (words.length == 1) {
      return americanNumberSystem.get(words.head)
    }
    try {
      // accumulate result here
      var totalSum: Double = 0
      var remainingWords = words.toArray
      for (w <- Seq("quadrillion", "trillion", "billion", "million", "thousand")) {
        val index = remainingWords.indexOf(w)
        if (index >= 0) {
          val multiplier = numberFormation(remainingWords.slice(0, index))
          remainingWords = remainingWords.drop(index + 1)
          totalSum += multiplier * americanNumberSystem(w)
        }
      }
      // handle hundreds
      totalSum += numberFormation(remainingWords)
      // return number
      Some(totalSum)
    } catch {
      case _: Exception => None
    }
  }

  def numberFormation(words: Array[String]): Double = {
    val numbers = ArrayBuffer.empty[Double]
    for (w <- words) {
       numbers += americanNumberSystem(w)
    }
    numbers.size match {
      case 4 => numbers(0) * numbers(1) + numbers(2) + numbers(3)
      case 3 => numbers(0) * numbers(1) + numbers(2)
      case 2 if numbers.contains(100) => numbers(0) * numbers(1)
      case 2 => numbers(0) + numbers(1)
      case 1 => numbers(0)
      case 0 => 0
    }
  }

  // https://en.wikipedia.org/wiki/Names_of_large_numbers
  val americanNumberSystem: Map[String, Double] = Map[String, Double](
    "zero"        -> 0,
    "one"         -> 1,
    "two"         -> 2,
    "three"       -> 3,
    "four"        -> 4,
    "five"        -> 5,
    "six"         -> 6,
    "seven"       -> 7,
    "eight"       -> 8,
    "nine"        -> 9,
    "ten"         -> 10,
    "eleven"      -> 11,
    "twelve"      -> 12,
    "thirteen"    -> 13,
    "fourteen"    -> 14,
    "fifteen"     -> 15,
    "sixteen"     -> 16,
    "seventeen"   -> 17,
    "eighteen"    -> 18,
    "nineteen"    -> 19,
    "twenty"      -> 20,
    "thirty"      -> 30,
    "forty"       -> 40,
    "fifty"       -> 50,
    "sixty"       -> 60,
    "seventy"     -> 70,
    "eighty"      -> 80,
    "ninety"      -> 90,
    "hundred"     -> 1e2,
    "thousand"    -> 1e3,
    "million"     -> 1e6,
    "billion"     -> 1e9,
    "trillion"    -> 1e12,
    "quadrillion" -> 1e15
  )

  val hyphenated = Set(
    "twenty-one", "twenty-two", "twenty-three", "twenty-four", "twenty-five", "twenty-six", "twenty-seven", "twenty-eight", "twenty-nine",
    "thirty-one", "thirty-two", "thirty-three", "thirty-four", "thirty-five", "thirty-six", "thirty-seven", "thirty-eight", "thirty-nine",
    "forty-one", "forty-two", "forty-three", "forty-four", "forty-five", "forty-six", "forty-seven", "forty-eight", "forty-nine",
    "fifty-one", "fifty-two", "fifty-three", "fifty-four", "fifty-five", "fifty-six", "fifty-seven", "fifty-eight", "fifty-nine",
    "sixty-one", "sixty-two", "sixty-three", "sixty-four", "sixty-five", "sixty-six", "sixty-seven", "sixty-eight", "sixty-nine",
    "seventy-one", "seventy-two", "seventy-three", "seventy-four", "seventy-five", "seventy-six", "seventy-seven", "seventy-eight", "seventy-nine",
    "eighty-one", "eighty-two", "eighty-three", "eighty-four", "eighty-five", "eighty-six", "eighty-seven", "eighty-eight", "eighty-nine",
    "ninety-one", "ninety-two", "ninety-three", "ninety-four", "ninety-five", "ninety-six", "ninety-seven", "ninety-eight", "ninety-nine"
  )

}
