package edu.arizona.sista.odin.impl

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._

class ActionMirror[T <: Actions : ClassTag](obj: T) {
  private val instanceMirror = runtimeMirror(obj.getClass.getClassLoader).reflect(obj)

  def reflect(name: String): ReflectedAction = {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(TermName(name)).asMethod
    val methodMirror = instanceMirror.reflectMethod(methodSymbol)
    new ReflectedAction(name, methodMirror)
  }
}

class ReflectedAction(val name: String, methodMirror: MethodMirror) {
  def apply(mention: Mention, state: State): Seq[Mention] =
    methodMirror(mention, state).asInstanceOf[Seq[Mention]]
}
