package org.clulab.odin.debugger.utils

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import java.util.{Map => JMap}
import scala.jdk.CollectionConverters._

object RuleUtils {
  val yaml = new Yaml(new Constructor(classOf[JMap[String, Any]]))

  def toMap(rule: String): Map[String, Any] = {
    val map = yaml.load(rule).asInstanceOf[JMap[String, Any]].asScala.toMap

    map
  }
}
