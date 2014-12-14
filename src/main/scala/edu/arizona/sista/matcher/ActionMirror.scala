package edu.arizona.sista.matcher

import scala.reflect.runtime.universe._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

class ActionMirror(obj: AnyRef) {
  private val instanceMirror = runtimeMirror(obj.getClass.getClassLoader).reflect(obj)

  def reflect(name: String): Action = {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(newTermName(name)).asMethod
    val methodMirror = instanceMirror.reflectMethod(methodSymbol)
    new Action(methodMirror)
  }
}

class Action(methodMirror: MethodMirror) {
  def apply(mention: Map[String, Seq[Interval]], sent: Int, doc: Document): Seq[Mention] =
    methodMirror(mention, sent, doc).asInstanceOf[Seq[Mention]]
}
