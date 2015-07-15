package edu.arizona.sista.odin.impl

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import edu.arizona.sista.odin._

class ActionMirror[A <: Actions : ClassTag](actions: A) {
  private val instanceMirror = runtimeMirror(actions.getClass.getClassLoader).reflect(actions)

  def reflect(name: String): Action = {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(TermName(name)).asMethod
    val methodMirror = instanceMirror.reflectMethod(methodSymbol)
    if (methodSymbol.returnType <:< typeOf[Action]) {
      methodMirror().asInstanceOf[Action]
    } else if (methodSymbol.returnType <:< typeOf[Seq[Mention]]) {
      (ms: Seq[Mention], st: State) => methodMirror(ms, st).asInstanceOf[Seq[Mention]]
    } else {
      sys.error(s"invalid action '$name'")
    }
  }
}
