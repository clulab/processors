package org.clulab.numeric

import scala.collection.mutable.ArrayBuffer

/**
  * Parses textual numbers, e.g., "twelve hundred", into numbers, e.g., "1200"
  */
object NumberParser {

  def parse(words: Seq[String]): Option[Double] = {
    words match {
      case Seq() => None
      case words =>
        val cleanWords = words.flatMap { w =>
          // lowercase
          var word = w.toLowerCase()
          // remove commas from numbers like 100,000
          word = word.replace(",", "")
          // remove 's' from words like "thousands"
          if (word.endsWith("s")) {
            word = word.dropRight(1)
          }
          // split on dashes
          hyphenated.getOrElse(word, Array(word))
        }
        parseWords(cleanWords) orElse parseNumeric(cleanWords)
    }
  }

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

  val hyphenated = Map(
    "twenty-one" -> Array("twenty", "one"),
    "twenty-two" -> Array("twenty", "two"),
    "twenty-three" -> Array("twenty", "three"),
    "twenty-four" -> Array("twenty", "four"),
    "twenty-five" -> Array("twenty", "five"),
    "twenty-six" -> Array("twenty", "six"),
    "twenty-seven" -> Array("twenty", "seven"),
    "twenty-eight" -> Array("twenty", "eight"),
    "twenty-nine" -> Array("twenty", "nine"),

    "thirty-one" -> Array("thirty", "one"),
    "thirty-two" -> Array("thirty", "two"),
    "thirty-three" -> Array("thirty", "three"),
    "thirty-four" -> Array("thirty", "four"),
    "thirty-five" -> Array("thirty", "five"),
    "thirty-six" -> Array("thirty", "six"),
    "thirty-seven" -> Array("thirty", "seven"),
    "thirty-eight" -> Array("thirty", "eight"),
    "thirty-nine" -> Array("thirty", "nine"),

    "forty-one" -> Array("forty", "one"),
    "forty-two" -> Array("forty", "two"),
    "forty-three" -> Array("forty", "three"),
    "forty-four" -> Array("forty", "four"),
    "forty-five" -> Array("forty", "five"),
    "forty-six" -> Array("forty", "six"),
    "forty-seven" -> Array("forty", "seven"),
    "forty-eight" -> Array("forty", "eight"),
    "forty-nine" -> Array("forty", "nine"),

    "fifty-one" -> Array("fifty", "one"),
    "fifty-two" -> Array("fifty", "two"),
    "fifty-three" -> Array("fifty", "three"),
    "fifty-four" -> Array("fifty", "four"),
    "fifty-five" -> Array("fifty", "five"),
    "fifty-six" -> Array("fifty", "six"),
    "fifty-seven" -> Array("fifty", "seven"),
    "fifty-eight" -> Array("fifty", "eight"),
    "fifty-nine" -> Array("fifty", "nine"),

    "sixty-one" -> Array("sixty", "one"),
    "sixty-two" -> Array("sixty", "two"),
    "sixty-three" -> Array("sixty", "three"),
    "sixty-four" -> Array("sixty", "four"),
    "sixty-five" -> Array("sixty", "five"),
    "sixty-six" -> Array("sixty", "six"),
    "sixty-seven" -> Array("sixty", "seven"),
    "sixty-eight" -> Array("sixty", "eight"),
    "sixty-nine" -> Array("sixty", "nine"),

    "seventy-one" -> Array("seventy", "one"),
    "seventy-two" -> Array("seventy", "two"),
    "seventy-three" -> Array("seventy", "three"),
    "seventy-four" -> Array("seventy", "four"),
    "seventy-five" -> Array("seventy", "five"),
    "seventy-six" -> Array("seventy", "six"),
    "seventy-seven" -> Array("seventy", "seven"),
    "seventy-eight" -> Array("seventy", "eight"),
    "seventy-nine" -> Array("seventy", "nine"),

    "eighty-one" -> Array("eighty", "one"),
    "eighty-two" -> Array("eighty", "two"),
    "eighty-three" -> Array("eighty", "three"),
    "eighty-four" -> Array("eighty", "four"),
    "eighty-five" -> Array("eighty", "five"),
    "eighty-six" -> Array("eighty", "six"),
    "eighty-seven" -> Array("eighty", "seven"),
    "eighty-eight" -> Array("eighty", "eight"),
    "eighty-nine" -> Array("eighty", "nine"),

    "ninety-one" -> Array("ninety", "one"),
    "ninety-two" -> Array("ninety", "two"),
    "ninety-three" -> Array("ninety", "three"),
    "ninety-four" -> Array("ninety", "four"),
    "ninety-five" -> Array("ninety", "five"),
    "ninety-six" -> Array("ninety", "six"),
    "ninety-seven" -> Array("ninety", "seven"),
    "ninety-eight" -> Array("ninety", "eight"),
    "ninety-nine" -> Array("ninety", "nine"),
  )

}
