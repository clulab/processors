package org.clulab.utils

import scala.math.Ordering

object Unordered {

  // For discussion see https://users.scala-lang.org/t/sorting-by-comparable-parts/7769.
  implicit class OrderingOrElseBy[T <: AnyRef](val ordering: Ordering[T]) {
    def orElseBy[U](f: T => U)(implicit innerOrdering: Ordering[U]): Ordering[T] = new Ordering[T] {
      def compare(x: T, y: T): Int = {
        if (x.eq(y)) 0 // Short-circuit all further comparisons.
        else {
          val result = ordering.compare(x, y)

          if (result == 0) innerOrdering.compare(f(x), f(y))
          else result
        }
      }
    }

    def orElse[U](default: Int): Ordering[T] = new Ordering[T] {
      def compare(x: T, y: T): Int = {
        val result = ordering.compare(x, y)

        if (result == 0) default
        else result
      }
    }
  }

  def apply[T]: Ordering[T] = new Ordering[T] {
    def compare(x: T, y: T): Int = 0
  }
}
