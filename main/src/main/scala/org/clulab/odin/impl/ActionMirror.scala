package org.clulab.odin.impl

import java.util.{ List => JList }
import scala.collection.JavaConverters._
import scala.reflect.runtime.universe._
import org.clulab.odin._

class ActionMirror(actions: Actions) {

  private val instanceMirror = runtimeMirror(actions.getClass.getClassLoader).reflect(actions)

  def debug(message: String, mentions: Seq[Mention], state: State): Unit = {

    def debugMentions(mentions: Seq[Mention]): Unit = {
      mentions.foreach { mention =>
        val hash = mention.hashCode
        val foundBy = mention.foundBy
        val label = mention.label.mkString(" ")
        val arguments = mention.arguments.keys.toSeq.sorted.mkString(" ")

        println(s"\thash: $hash, foundBy: $foundBy, label: $label, arguments: $arguments")
      }
    }

    def debugState(state: State): Unit = {

    }

    println(message)
    debugMentions(mentions)
    debugState(state)
    println()
  }

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

      debug(s"INFO: Entering action $name with ${mentions.length} mentions:", mentions, state)
      val result = function(mentions, state)
      debug(s"INFO: Exiting action $name with ${mentions.length} mentions:", mentions, state)
      result
    }
  }

}
