package edu.arizona.sista.utils

import java.io.{ InputStream, ObjectInputStream, ObjectStreamClass }

class ClassLoaderObjectInputStream(cl: ClassLoader, is: InputStream) extends ObjectInputStream(is) {
  override def resolveClass(osc: ObjectStreamClass): Class[_] = {
    val c = Class.forName(osc.getName, false, cl)
    if (c != null) c else super.resolveClass(osc)
  }
}
