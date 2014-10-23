package edu.arizona.sista.matcher

import scala.reflect.runtime.universe._
import edu.arizona.sista.processors.Document

class ActionMirror(obj: AnyRef) {
  private val instanceMirror = runtimeMirror(obj.getClass.getClassLoader).reflect(obj)

  def invoke(name: String, document: Document, sentence: Int, mentions: Map[String, Seq[Mention]]): Seq[Mention] = {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(newTermName(name)).asMethod
    val methodMirror = instanceMirror.reflectMethod(methodSymbol)
    methodMirror(document, sentence, mentions).asInstanceOf[Seq[Mention]]
  }
}
