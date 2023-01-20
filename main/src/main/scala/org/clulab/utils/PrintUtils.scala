package org.clulab.utils

import java.io.{PrintStream, PrintWriter}

object PrintUtils {
  val empty = ""
  val arrayStart = "Array("
  val arraySep = ", "
  val arrayEnd = ")"

  abstract class Printing {

    def print(any: Any): Unit

    def println(any: Any): Unit = {
      print(any)
      println()
    }

    def println(): Unit
  }

  implicit class PrintingStream(printStream: PrintStream) extends Printing {

    def print(any: Any): Unit = printStream.print(any)

    def println(): Unit = printStream.println()
  }

  implicit class PrintintWriter(printWriter: PrintWriter) extends Printing {

    def print(any: Any): Unit = printWriter.print(any)

    def println(): Unit = printWriter.println()
  }

  implicit class Printer(any: Any) {

    protected def asArrayOpt(any: Any): Option[Array[_]] = any match {
      case array: Array[_] => Some(array)
      case _ => None
    }

    def print(): Unit = asArrayOpt(any)
        .map(_.view.print(arrayStart, arraySep, arrayEnd))
        .getOrElse(System.out.print(any))
    def print(sep: String): Unit = print(empty, sep, empty)
    def print(start: String, sep: String, end: String): Unit =
        print(System.out, start, sep, end)

    def print(printing: Printing): Unit = asArrayOpt(any)
        .map(_.view.print(printing, arrayStart, arraySep, arrayEnd))
        .getOrElse(printing.print(any))
    def print(printing: Printing, sep: String): Unit = print(printing, empty, sep, empty)
    def print(printing: Printing, start: String, sep: String, end: String): Unit =
        execute(printing, start, sep, end, newline = false)

    def println(): Unit = asArrayOpt(any)
        .map(_.view.println(arrayStart, arraySep, arrayEnd))
        .getOrElse(System.out.println(any))
    def println(sep: String): Unit = println(empty, sep, empty)
    def println(start: String, sep: String, end: String): Unit =
        println(System.out, start, sep, end)

    def println(printing: Printing): Unit = asArrayOpt(any)
        .map(_.view.println(printing, arrayStart, arraySep, arrayEnd))
        .getOrElse(printing.println(any))
    def println(printing: Printing, sep: String): Unit = println(printing, empty, sep, empty)
    def println(printing: Printing, start: String, sep: String, end: String): Unit =
        execute(printing, start, sep, end, newline = true)

    protected def execute(printing: Printing, start: String, sep: String, end: String, newline: Boolean): Unit = {
      val patchedAny = asArrayOpt(any).map(_.view).getOrElse(any)

      if (start.nonEmpty) printing.print(start)
      patchedAny match {
        case iterable: Iterable[_] =>
          var started = false
          iterable.foreach { each =>
            if (started) printing.print(sep)
            else started = true

            printing.print(each)
          }
        case other => printing.print(other)
      }
      if (end.nonEmpty) printing.print(end)
      if (newline) printing.println()
    }
  }
}
