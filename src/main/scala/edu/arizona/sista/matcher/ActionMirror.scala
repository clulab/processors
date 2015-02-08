package edu.arizona.sista.matcher

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

class ActionMirror[T <: Actions : ClassTag](obj: T) {
  private val instanceMirror = runtimeMirror(obj.getClass.getClassLoader).reflect(obj)

  def reflect(name: String): ReflectedAction = ActionMirror.Lock.synchronized {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(TermName(name)).asMethod
    val methodMirror = instanceMirror.reflectMethod(methodSymbol)
    new ReflectedAction(name, methodMirror)
  }
}

object ActionMirror {
  private object Lock  // scala reflection isn't thread-safe :(
}

class ReflectedAction(val name: String, methodMirror: MethodMirror) {
  def apply(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State, keep: Boolean): Seq[Mention] =
    methodMirror(label, mention, sent, doc, ruleName, state, keep).asInstanceOf[Seq[Mention]]
}
