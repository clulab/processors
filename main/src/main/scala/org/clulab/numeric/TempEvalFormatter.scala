package org.clulab.numeric

import NumberParser._

object TempEvalFormatter {
  val NO_VALUE = "XXXX"

  def mkDate(day: Option[Seq[String]], month: Option[Seq[String]], year: Option[Seq[String]]): String = {
    val dayValue =
      if(day.isEmpty) NO_VALUE
      else parse(day.get).toInt.toString

    val monthValue =
      if(month.isEmpty) NO_VALUE
      else if(month.get.head(0).isLetter) convertLiteralMonth(month.get.head).toString
      else parse(month.get).toInt.toString

    val yearValue =
      if(year.isEmpty) NO_VALUE
      else parse(year.get).toInt.toString

    s"$yearValue-$monthValue-$dayValue"
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
}
