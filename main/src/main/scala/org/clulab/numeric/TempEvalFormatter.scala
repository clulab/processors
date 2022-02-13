package org.clulab.numeric

import NumberParser._

object TempEvalFormatter {

  def mkDate(day: Option[Seq[String]],
             month: Option[Seq[String]],
             year: Option[Seq[String]],
             modifierSymbol: Option[String] = None): String = {
    val dayValue =
      if(day.isEmpty) {
        0
      } else {
        parse(day.get).get.toInt
      }

    val monthValue =
      if(month.isEmpty) 0
      else if(month.get.head(0).isLetter) convertLiteralMonth(month.get.head)
      else parse(month.get).get.toInt

    val yearValue =
      if(year.isEmpty) 0
      else parse(year.get).get.toInt

    val dayAsString = formatNumber(dayValue, 2)
    val monthAsString = formatNumber(monthValue, 2)
    val yearAsString = formatNumber(yearValue, 4, "X")

    if (modifierSymbol.isDefined)
      s"$yearAsString-$monthAsString-$dayAsString ${modifierSymbol.get}"
    else
      s"$yearAsString-$monthAsString-$dayAsString"
  }

  private def formatNumber(v: Int, length: Int, padding: String = "0"): String = {
    if(v == 0) "X" * length
    else {
      val content = v.toString
      val paddingLength = length - content.length
      if(paddingLength > 0) {
        // println(s"paddingLength for $v is $paddingLength")
        val sb = new StringBuilder
        sb.append(padding * paddingLength)
        sb.append(content)
        sb.toString()
      } else {
        content
      }
    }
  }

  private def convertLiteralMonth(s: String): Int = {
    val v = s.toLowerCase()

    if(v.startsWith("jan")) 1
    else if(v.startsWith("feb")) 2
    else if(v.startsWith("mar")) 3
    else if(v.startsWith("apr")) 4
    else if(v.startsWith("may")) 5
    else if(v.startsWith("jun")) 6
    else if(v.startsWith("jul")) 7
    else if(v.startsWith("aug")) 8
    else if(v.startsWith("sep")) 9
    else if(v.startsWith("oct")) 10
    else if(v.startsWith("nov")) 11
    else if(v.startsWith("dec")) 12
    else throw new RuntimeException(s"ERROR: unknown month $s!")
  }

  def mkDate1Norm(number: Double, dateNorm: String): String = {
    // replace the day value with number
    val lastHyphen = dateNorm.lastIndexOf('-')
    val prefix = dateNorm.substring(0, lastHyphen + 1)

    val newDay = formatNumber(number.toInt, 2)
    prefix + newDay
  }
}
