package edu.arizona.sista.matcher

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

class ActionMirror[T <: Actions : ClassTag](obj: T) {
  private val instanceMirror = runtimeMirror(obj.getClass.getClassLoader).reflect(obj)

  def reflect(name: String): Action = {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(newTermName(name)).asMethod
    val methodMirror = instanceMirror.reflectMethod(methodSymbol)
    new Action(name, methodMirror)
  }
}

class Action(val name: String, methodMirror: MethodMirror) {
  def apply(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] =
    methodMirror(label, mention, sent, doc, ruleName, state).asInstanceOf[Seq[Mention]]
}
