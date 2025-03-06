package org.clulab.odin.debugger.utils

class EqualityByIdentity[T](val value: T) extends Equality {

  override def hashCode(): Int = value.hashCode()

  override def equals(other: Any): Boolean =
    if (other.isInstanceOf[EqualityByIdentity[_]])
      (this.value, other.asInstanceOf[EqualityByIdentity[_]].value) match {
        case (self: Byte, other: Byte) => self == other
        case (self: Short, other: Short) => self == other
        case (self: Int, other: Int) => self == other
        case (self: Long, other: Long) => self == other
        case (self: Float, other: Float) => self == other
        case (self: Double, other: Double) => self == other
        case (self: AnyRef, other: AnyRef) =>
          val result = self.eq(other)
          result
        case _ => false
      }
    else false
}

object EqualityByIdentity {

  def apply[T](value: T): EqualityByIdentity[T] = new EqualityByIdentity(value)
}
