package edu.arizona.sista.matcher

import scala.reflect.runtime.universe._

class ActionMirror(obj: AnyRef) {
  private val instanceMirror = runtimeMirror(obj.getClass.getClassLoader).reflect(obj)

  def invoke(name: String, args: Int): Int = {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(newTermName(name)).asMethod
    val methodMirror = instanceMirror.reflectMethod(methodSymbol)
    methodMirror(args).asInstanceOf[Int]
  }
}
