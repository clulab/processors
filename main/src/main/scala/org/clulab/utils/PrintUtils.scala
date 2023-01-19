package org.clulab.utils

import java.io.{PrintStream, PrintWriter}

object PrintUtils {
  val empty = ""
  val arrayStart = "Array("
  val arraySep = ", "
  val arrayEnd = ")"

  implicit class Printer(any: Any) {

    protected abstract class Printable {

      def print(any: Any): Unit

      def println(): Unit
    }

    case class PrintableStream(printStream: PrintStream) extends Printable {

      def print(any: Any): Unit = printStream.print(any)

      def println(): Unit = printStream.println()
    }

    case class PrintableWriter(printWriter: PrintWriter) extends Printable {

      def print(any: Any): Unit = printWriter.print(any)

      def println(): Unit = printWriter.println()
    }

    protected def asArrayOpt(any: Any): Option[Array[_]] =
        if (any.isInstanceOf[Array[_]]) Some(any.asInstanceOf[Array[_]])
        else None

    def print: Unit = asArrayOpt(any)
        .map(_.toIterable.print(arrayStart, arraySep, arrayEnd))
        .getOrElse(System.out.print(any))
    def print(sep: String): Unit = print(empty, sep, empty)
    def print(start: String, sep: String, end: String): Unit =
        print(System.out, start, sep, end)

    def print(printStream: PrintStream): Unit = asArrayOpt(any)
        .map(_.toIterable.print(printStream, arrayStart, arraySep, arrayEnd))
        .getOrElse(printStream.print(any))
    def print(printStream: PrintStream, sep: String): Unit = print(printStream, empty, sep, empty)
    def print(printStream: PrintStream, start: String, sep: String, end: String): Unit =
        execute(PrintableStream(printStream), start, sep, end, false)

    def print(printWriter: PrintWriter): Unit = asArrayOpt(any)
        .map(_.toIterable.print(printWriter, arrayStart, arraySep, arrayEnd))
        .getOrElse(printWriter.print(any))
    def print(printWriter: PrintWriter, sep: String): Unit = print(printWriter, empty, sep, empty)
    def print(printWriter: PrintWriter, start: String, sep: String, end: String): Unit =
        execute(PrintableWriter(printWriter), start, sep, end, false)

    def println: Unit = asArrayOpt(any)
        .map(_.toIterable.println(arrayStart, arraySep, arrayEnd))
        .getOrElse(System.out.println(any))
    def println(sep: String): Unit = println(empty, sep, empty)
    def println(start: String, sep: String, end: String): Unit =
        println(System.out, start, sep, end)

    def println(printStream: PrintStream): Unit = asArrayOpt(any)
        .map(_.toIterable.println(printStream, arrayStart, arraySep, arrayEnd))
        .getOrElse(printStream.println(any))
    def println(printStream: PrintStream, sep: String): Unit = println(printStream, empty, sep, empty)
    def println(printStream: PrintStream, start: String, sep: String, end: String): Unit =
        execute(PrintableStream(printStream), start, sep, end, true)

    def println(printWriter: PrintWriter): Unit = asArrayOpt(any)
        .map(_.toIterable.println(printWriter, arrayStart, arraySep, arrayEnd))
        .getOrElse(printWriter.println(any))
    def println(printWriter: PrintWriter, sep: String): Unit = println(printWriter, empty, sep, empty)
    def println(printWriter: PrintWriter, start: String, sep: String, end: String): Unit =
        execute(PrintableWriter(printWriter), start, sep, end, true)

    protected def execute(printable: Printable, start: String, sep: String, end: String, newline: Boolean): Unit = {
      val patchedAny = asArrayOpt(any).map(_.toIterable).getOrElse(any)

      if (start.nonEmpty) printable.print(start)
      patchedAny match {
        case iterable: Iterable[_] =>
          var started = false
          iterable.foreach { each =>
            if (started) printable.print(sep)
            else started = true

            printable.print(each)
          }
        case other => printable.print(other)
      }
      if (end.nonEmpty) printable.print(end)
      if (newline) printable.println()
    }
  }
}
