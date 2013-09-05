package edu.arizona.sista.utils

/**
 * Stores a mutable number of type T
 * User: mihais
 * Date: 3/18/13
 */
class MutableNumber[T](var value:T) {
  override def hashCode = value.hashCode

  override def equals(other:Any):Boolean = {
    other match {
      case that:MutableNumber[T] => value == that.value
      case _ => false
    }
  }
}
