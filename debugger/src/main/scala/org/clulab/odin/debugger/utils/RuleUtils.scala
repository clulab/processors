package org.clulab.odin.debugger.utils

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import java.util.{Map => JMap}
import scala.jdk.CollectionConverters._

object RuleUtils {
  type JavaRule = JMap[String, Any]
  type ScalaRule = Map[String, Any]

  val yaml = new Yaml(new Constructor(classOf[JavaRule]))

  def toScalaRule(rule: String): ScalaRule = {
    val javaRule = yaml.load(rule).asInstanceOf[JavaRule]
    val scalaRule = javaRule.asScala.toMap

    scalaRule
  }

  def toRule(scalaRule: ScalaRule): String = {
    val rule = yaml.dump(scalaRule.asJava)

    rule
  }
}
