package edu.arizona.sista.matcher

import scala.reflect.runtime.universe._

class ActionMirror(obj: AnyRef) {
  private val instanceMirror = runtimeMirror(obj.getClass.getClassLoader).reflect(obj)

  def reflect(name: String): Action = {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(newTermName(name)).asMethod
    val methodMirror = instanceMirror.reflectMethod(methodSymbol)
    new Action(methodMirror)
  }
}

class Action(methodMirror: MethodMirror) {
  def apply(sent: Int, state: State, found: Map[String, Seq[Int]]): Seq[Mention] =
    methodMirror(sent, state, found).asInstanceOf[Seq[Mention]]
}
