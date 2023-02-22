package org.clulab.odin.impl

import org.clulab.odin.Actions
import org.clulab.odin.impl.RuleReader.Rule
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import java.nio.charset.Charset
import java.util.{Collection, Map => JMap}

/** This class addresses [[https://github.com/clulab/processors/issues/309]]
  *
  * Note: nothing is synchronized here, so don't manipulate the configs in a multi-
  * threaded environment.
  */
class CustomRuleReader(actions: Actions, charset: Charset) extends RuleReader(actions, charset) {
  /** whether the circumstances are right to capture the config in [[readRules]] */
  protected var captureConfig: Boolean = false
  /** most-recent config generated in [[rulesFromMasterFile]] and then captured */
  protected var config: OdinConfig = OdinConfig(resources = OdinResourceManager(Map.empty))

  /** Override that reuses the captured config */
  override protected def rulesFromSimpleFile(input: String): Seq[Rule] = {
    val yaml = new Yaml(new Constructor(classOf[Collection[JMap[String, Any]]]))
    val jRules = yaml.load(input).asInstanceOf[Collection[JMap[String, Any]]]

    readRules(jRules, this.config)
  }

  /** Override that enables the config to be captured */
  override def rulesFromMasterFile(input: String): Seq[Rule] = {
    // The superclass's version calls readRules and when this happens, we want the config
    // to be captured.  This saves us from reimplementation of the superclass's method.
    captureConfig = true
    super.rulesFromMasterFile(input)
  }

  /** Override that *captures* the [[OdinConfig]] as a side-effect */
  override protected def readRules(rules: Collection[JMap[String, Any]], config: OdinConfig): Seq[Rule] = {
    if (captureConfig) {
      this.config = config
      captureConfig = false
    }
    super.readRules(rules, config)
  }
}
