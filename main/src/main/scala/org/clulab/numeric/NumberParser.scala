package org.clulab.numeric

import scala.collection.mutable.ArrayBuffer

/**
  * Parses textual numbers, e.g., "twelve hundred", into numbers, e.g., "1200"
  */
object NumberParser {

  def parse(words: Seq[String]): Option[Double] = {

    if (words.size == 1 && words(0).size > 0 && allDigits(words(0))) {
      return Some(words(0).toInt)
    }

    // accumulate result here
    var totalSum: Double = 0

    // discard tokens that don't look like numbers
    val cleanWords = cleanNumber(words)
    if (cleanWords.isEmpty) return None

    // confirm 'thousand', 'million', and 'billion' appear in the correct order
    val billionIndex = cleanWords.indexOf("billion")
    val millionIndex = cleanWords.indexOf("million")
    val thousandIndex = cleanWords.indexOf("thousand")
    if ((thousandIndex > -1 && (thousandIndex < millionIndex || thousandIndex < billionIndex)) || (millionIndex > -1 && millionIndex < billionIndex)) {
      return None
    }

    if (cleanWords.size == 1) {
      totalSum += americanNumberSystem(cleanWords(0))
    }
    else {
      if (billionIndex > -1) {
        val billionMultiplier = numberFormation(cleanWords.slice(0, billionIndex))
        totalSum += billionMultiplier * americanNumberSystem("billion")
      }
      if (millionIndex > -1) {
        var millionMultiplier: Int = 0
        if (billionIndex > -1) {
          millionMultiplier = numberFormation(cleanWords.slice(billionIndex+1, millionIndex))
        } else {
          millionMultiplier = numberFormation(cleanWords.slice(0, millionIndex))
        }
        totalSum += millionMultiplier * americanNumberSystem("million")
      }
      if (thousandIndex > -1) {
        var thousandMultiplier: Int = 0
        if (millionIndex > -1) {
          thousandMultiplier = numberFormation(cleanWords.slice(millionIndex+1, thousandIndex))
        } else if (billionIndex > -1 && millionIndex == -1) {
          thousandMultiplier = numberFormation(cleanWords.slice(billionIndex+1, thousandIndex))
        } else {
          thousandMultiplier = numberFormation(cleanWords.slice(0, thousandIndex))
        }
        totalSum += thousandMultiplier * americanNumberSystem("thousand")
      }
      var hundreds: Int = 0
      if (thousandIndex > -1 && thousandIndex != cleanWords.size-1) {
        hundreds = numberFormation(cleanWords.slice(thousandIndex+1, cleanWords.size))
      } else if (millionIndex > -1 && millionIndex != cleanWords.size-1) {
        hundreds = numberFormation(cleanWords.slice(millionIndex+1, cleanWords.size))
      } else if (billionIndex > -1 && billionIndex != cleanWords.size-1) {
        hundreds = numberFormation(cleanWords.slice(billionIndex+1, cleanWords.size))
      } else if (thousandIndex == -1 && millionIndex == -1 && billionIndex == -1) {
        hundreds = numberFormation(cleanWords)
      }
      totalSum += hundreds
    }

  return Some(totalSum)

  }

  def cleanNumber(words: Seq[String]): Array[String] = {
    val cleanWords = ArrayBuffer.empty[String]
    for (word <- words) {
      val w = word.toLowerCase()
      if (americanNumberSystem contains w) {
        cleanWords += w
      }
    }
    return cleanWords.toArray
  }

  def numberFormation(words: Array[String]): Int = {
    val numbers = ArrayBuffer.empty[Int]
    for (w <- words) {
      numbers += americanNumberSystem(w)
    }
    numbers.size match {
      case 4 => numbers(0) * numbers(1) + numbers(2) + numbers(3)
      case 3 => numbers(0) * numbers(1) + numbers(2)
      case 2 if numbers.contains(100) => numbers(0) * numbers(1)
      case 2 => numbers(0) + numbers(1)
      case 1 => numbers(0)
    }
  }

  def allDigits(s: String): Boolean = {
    s.forall(_.isDigit)
  }

  val decimalWords = Array("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  // note that billions have 9 zeros
  val americanNumberSystem = Map[String, Int](
    "zero"      -> 0,
    "one"       -> 1,
    "two"       -> 2,
    "three"     -> 3,
    "four"      -> 4,
    "five"      -> 5,
    "six"       -> 6,
    "seven"     -> 7,
    "eight"     -> 8,
    "nine"      -> 9,
    "ten"       -> 10,
    "eleven"    -> 11,
    "twelve"    -> 12,
    "thirteen"  -> 13,
    "fourteen"  -> 14,
    "fifteen"   -> 15,
    "sixteen"   -> 16,
    "seventeen" -> 17,
    "eighteen"  -> 18,
    "nineteen"  -> 19,
    "twenty"    -> 20,
    "thirty"    -> 30,
    "forty"     -> 40,
    "fifty"     -> 50,
    "sixty"     -> 60,
    "seventy"   -> 70,
    "eighty"    -> 80,
    "ninety"    -> 90,
    "hundred"   -> 100,
    "thousand"  -> 1000,
    "million"   -> 1000000,
    "billion"   -> 1000000000
  )

}
