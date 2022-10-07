package org.clulab.odin.impl

import scala.collection.mutable

object Namer {
  val names: mutable.Map[String, Int] = new mutable.HashMap()

  def name(value: Any): String = synchronized {
    val className = value.getClass.getName
    val count = names.getOrElse(className, 0)
    val result = s"$className.$count"

    names.put(className, count + 1)
    println(s"[Namer] Created name $result")
    result
  }
}
