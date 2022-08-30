package org.clulab.odin.impl

import java.util.{ List => JList }
import scala.collection.JavaConverters._
import scala.reflect.runtime.universe._
import org.clulab.odin._

class ActionMirror(actions: Actions) {

  private val instanceMirror = runtimeMirror(actions.getClass.getClassLoader).reflect(actions)

  def debug(name: String, entering: Boolean, mentions: Seq[Mention], state: State): Unit = {
    val message = s"INFO: ${if (entering) "Entering" else "Exiting"} action $name with ${mentions.length} mentions:"

    def debugMentions(level: Int, keyOpt: Option[String], mentions: Seq[Mention]): Unit = {
      mentions.foreach { mention =>
        val hash = mention.hashCode
        val foundBy = mention.foundBy
        val label = mention.labels.mkString(" ")
        val tags = mention.sentenceObj.tags.getOrElse(Array.empty).mkString(" ")
        val norms = mention.sentenceObj.norms.getOrElse(Array.empty).mkString(" ")
        val chunks = mention.sentenceObj.chunks.getOrElse(Array.empty).mkString(" ")
        val entities = mention.sentenceObj.entities.getOrElse(Array.empty).mkString(" ")

        Range(0, level).foreach(_ => print("\t"))
        println(s"\thash: $hash, key: $keyOpt, foundBy: $foundBy, label: $label, tags: $tags, norms: $norms, chunks: $chunks, entities: $entities")

        mention.arguments.foreach { case(key, value) =>
          debugMentions(level + 1, Some(key), value)
        }
      }
    }

    def debugState(state: State): Unit = {
      val message = s"INFO: ${if (entering) "Entering" else "Exiting"} action $name with state:"

      println(message)
      debugMentions(0, None, state.allMentions)
    }

    println(message)
    debugMentions(0, None, mentions)
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
      debug(name, true, mentions, state)
      val result = function(mentions, state)
      debug(name, false, result, state)
      result
    }
  }

}
