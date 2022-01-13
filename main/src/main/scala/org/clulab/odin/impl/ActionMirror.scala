package org.clulab.odin.impl

import org.clulab.odin._

import java.util.{ List => JList }
import scala.collection.JavaConverters._
import scala.reflect.runtime.universe._

class ActionMirror(actions: Actions) {
  private val instanceMirror = runtimeMirror(actions.getClass.getClassLoader).reflect(actions)

  def reflect(name: String): Action = {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(TermName(name)).asMethod
    val action = instanceMirror.reflectMethod(methodSymbol)
    // handle action based on its return type
    val returnType = methodSymbol.returnType

    if (returnType =:= ActionMirror.plainReturnType)
      // val action: Action
      action().asInstanceOf[Action]
    else if (returnType =:= ActionMirror.scalaReturnType)
      // def action(mentions: Seq[Mention], state: State): Seq[Mention]
      (mentions: Seq[Mention], state: State) => {
        action(mentions, state).asInstanceOf[Seq[Mention]]
      }
    else if (returnType =:= ActionMirror.javaReturnType)
      // java.util.List<Mention> action(java.util.List<Mention> mentions, State state)
      (mentions: Seq[Mention], state: State) => {
        action(mentions.asJava, state).asInstanceOf[JList[Mention]].asScala
      }
    else
      sys.error(s"invalid action '$name'")
  }
}

object ActionMirror {
  val plainReturnType: Type = typeOf[Action]
  val scalaReturnType: Type = typeOf[Seq[Mention]]
  val javaReturnType: Type = typeOf[JList[Mention]]
}
