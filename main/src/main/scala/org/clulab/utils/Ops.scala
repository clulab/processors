package org.clulab.utils

import scala.reflect.ClassTag

object Ops {
  implicit class ObjectOps(any: Any) {
    def asInstanceOfOption[A](implicit tag: ClassTag[A]): Option[A] = tag.unapply(any)
  }
}
