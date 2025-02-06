package org.clulab.odin.debugger.utils

class EqualityByIdentity(val any: Any) {

  override def hashCode(): Int = any.hashCode()

  override def equals(other: Any): Boolean =
    if (other.isInstanceOf[EqualityByIdentity])
      (this.any, other.asInstanceOf[EqualityByIdentity].any) match {
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

  def apply(any: Any): EqualityByIdentity = new EqualityByIdentity(any)
}
