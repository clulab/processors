package org.clulab.odin.impl

import org.clulab.odin.Action
import org.clulab.odin.Actions
import org.clulab.odin.Mention
import org.clulab.odin.State

import scala.jdk.CollectionConverters._
import java.lang.reflect.Method
import java.util.{List => JList}

class ActionMirror(actions: Actions) {
  val methods: Array[Method] = actions.getClass.getMethods

  def reflect(name: String): Action = {
    val method: Method = methods.find { method =>
      method.getName == name
    }.get
    val returnType = method.getReturnType.getTypeName

    returnType match {
      case ActionMirror.plainReturnType =>
        // val action: Action
        method.invoke(actions).asInstanceOf[Action]
      case ActionMirror.scalaReturnType =>
        // def action(mentions: Seq[Mention], state: State): Seq[Mention]
        (mentions: Seq[Mention], state: State) => {
          method.invoke(actions, mentions, state).asInstanceOf[Seq[Mention]]
        }
      case ActionMirror.javaReturnType =>
        // java.util.List<Mention> action(java.util.List<Mention> mentions, State state)
        (mentions: Seq[Mention], state: State) => {
          method.invoke(actions, mentions.asJava, state).asInstanceOf[JList[Mention]].asScala.toSeq
        }
      case _ =>
        sys.error(s"invalid action '$name'")
    }
  }
}

object ActionMirror {
  val plainReturnType: String = classOf[Action].getTypeName
  val scalaReturnType: String = classOf[Seq[Mention]].getTypeName
  val javaReturnType: String = classOf[JList[Mention]].getTypeName
}
