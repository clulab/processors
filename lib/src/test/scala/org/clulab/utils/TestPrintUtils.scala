package org.clulab.utils

import org.clulab.utils.PrintUtils._

class TestPrintUtils extends Test {
  val int = 5
  val string = "hello"
  val array = Array(1, 2, 3)
  val list = List(11, 12, 13)
  val map = Map("a" -> 21, "b" -> 22, "c" -> 23)
  val seq = Seq(31, 32, 33)
  val tuple = ("a", 42, 43)

  behavior of "PrintUtils"

  it should "print with no arguments" in {

    def test(any: Any, expectedResult: String): Unit = {
      val standardResult = StringUtils.viaPrintWriter(_.print(any))
      val customResult = StringUtils.viaPrintWriter { printWriter => any.print(printWriter) }

      println(standardResult)
      println(customResult)

      customResult should be (expectedResult)
    }

    test(int, "5")
    test(string, "hello")
    test(array, "Array(1, 2, 3)")
    test(list, "List(11, 12, 13)")
    test(map, "Map(a -> 21, b -> 22, c -> 23)")
    test(seq, "List(31, 32, 33)")
    test(tuple, "(a,42,43)")
  }

  it should "print with separators" in {
    val start = "<"
    val sep = "-"
    val end = ">"

    def test(any: Any, expectedResult: String): Unit = {
      val customResult = StringUtils.viaPrintWriter { printWriter => any.print(printWriter, start, sep, end) }

      println(customResult)
      customResult should be (expectedResult)
    }

    test(int, "<5>")
    test(string, "<hello>")
    test(array, "<1-2-3>")
    test(list, "<11-12-13>")
    test(map, "<(a,21)-(b,22)-(c,23)>")
    test(seq, "<31-32-33>")
    test(tuple, "<(a,42,43)>")
  }
}
