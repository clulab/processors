package org.clulab.odin.impl

import java.util.{ List => JList }
import scala.collection.JavaConverters._
import scala.reflect.runtime.universe._
import org.clulab.odin._

class ActionMirror(actions: Actions) {

  private val instanceMirror = runtimeMirror(actions.getClass.getClassLoader).reflect(actions)

  def reflect(name: String): Action = {
    val methodSymbol = instanceMirror.symbol.typeSignature.member(TermName(name)).asMethod
    val action = instanceMirror.reflectMethod(methodSymbol)
    // handle action based on its return type
    val returnType = methodSymbol.returnType
    val function = if (returnType =:= typeOf[Action]) {
      // val action: Action
      action().asInstanceOf[Action]
    } else if (returnType =:= typeOf[Seq[Mention]]) {
      // def action(mentions: Seq[Mention], state: State): Seq[Mention]
      (mentions: Seq[Mention], state: State) => {
        action(mentions, state).asInstanceOf[Seq[Mention]]
      }
    } else if (returnType =:= typeOf[JList[Mention]]) {
      // java.util.List<Mention> action(java.util.List<Mention> mentions, State state)
      (mentions: Seq[Mention], state: State) => {
        action(mentions.asJava, state).asInstanceOf[JList[Mention]].asScala
      }
    } else {
      sys.error(s"invalid action '$name'")
    }
    (mentions: Seq[Mention], state: State) => {
      val keyCount = state.lookUpTable.keys.size
      val valueCount = state.lookUpTable.values.size
      val cellCount = state.lookUpTable.values.map(_.length).sum

//      if (mentions.isEmpty)
        println(s"WARNING: Entering $name with ${mentions.length}.")
      val result = function(mentions, state)
//      if (mentions.nonEmpty && result.isEmpty)
        println(s"WARNING: Exiting $name with ${mentions.length}.")
      result
    }
  }

}
