package edu.arizona.sista.odin.impl

import java.util.{ List => JList }
import scala.collection.JavaConverters._
import scala.reflect.runtime.universe._
import edu.arizona.sista.odin._

class ActionMirror(actions: Actions) {

  private val instanceMirror = runtimeMirror(actions.getClass.getClassLoader).reflect(actions)

  def reflect(name: String): Action = {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(TermName(name)).asMethod
    val methodMirror = instanceMirror.reflectMethod(methodSymbol)
    // handle action based on its return type
    val returnType = methodSymbol.returnType
    if (returnType =:= typeOf[Action]) {
      // val action: Action
      methodMirror().asInstanceOf[Action]
    } else if (returnType =:= typeOf[Seq[Mention]]) {
      // def action(mentions: Seq[Mention], state: State): Seq[Mention]
      (ms: Seq[Mention], st: State) => {
        methodMirror(ms, st).asInstanceOf[Seq[Mention]]
      }
    } else if (returnType =:= typeOf[JList[Mention]]) {
      // java.util.List<Mention> action(java.util.List<Mention> mentions, State state)
      (ms: Seq[Mention], st: State) => {
        methodMirror(ms.asJava, st).asInstanceOf[JList[Mention]].asScala
      }
    } else {
      sys.error(s"invalid action '$name'")
    }
  }

}
