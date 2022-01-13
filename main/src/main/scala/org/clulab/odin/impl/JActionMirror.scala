package org.clulab.odin.impl

import org.clulab.odin.Action
import org.clulab.odin.Actions
import org.clulab.odin.Mention
import org.clulab.odin.State

import scala.collection.JavaConverters._
import java.lang.reflect.Method
import java.util.{List => JList}

class JActionMirror(actions: Actions) {
  val methods: Array[Method] = actions.getClass.getMethods

  def reflect(name: String): Action = {
    val method: Method = methods.find { method =>
      method.getName == name
    }.get
    val returnType = method.getReturnType.getTypeName

    returnType match {
      case JActionMirror.plainReturnType =>
        // val action: Action
        method.invoke(actions).asInstanceOf[Action]
      case JActionMirror.scalaReturnType =>
        // def action(mentions: Seq[Mention], state: State): Seq[Mention]
        (mentions: Seq[Mention], state: State) => {
          method.invoke(actions, mentions, state).asInstanceOf[Seq[Mention]]
        }
      case JActionMirror.javaReturnType =>
        // java.util.List<Mention> action(java.util.List<Mention> mentions, State state)
        (mentions: Seq[Mention], state: State) => {
          method.invoke(actions, mentions.asJava, state).asInstanceOf[JList[Mention]].asScala
        }
      case _ =>
        sys.error(s"invalid action '$name'")
    }
  }
}

object JActionMirror {
  val plainReturnType: String = classOf[Action].getTypeName
  val scalaReturnType: String = classOf[Seq[Mention]].getTypeName
  val javaReturnType: String = classOf[JList[Mention]].getTypeName
}
