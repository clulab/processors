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
      case JActionMirror.actionReturnType =>
        // val action: Action
        (mentions: Seq[Mention], state: State) => {
          method.invoke(actions).asInstanceOf[Action](mentions, state).asInstanceOf[Seq[Mention]]
        }
      case JActionMirror.seqMentionReturnType =>
        // def action(mentions: Seq[Mention], state: State): Seq[Mention]
        (mentions: Seq[Mention], state: State) => {
          method.invoke(actions, mentions, state).asInstanceOf[Seq[Mention]]
        }
      case JActionMirror.jListMentionReturnType =>
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
  // Turn this into a map with converters which take the method.
  val actionReturnType: String = classOf[Action].getTypeName
  val seqMentionReturnType: String = classOf[Seq[Mention]].getTypeName
  val jListMentionReturnType: String = classOf[JList[Mention]].getTypeName
}
