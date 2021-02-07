package org.clulab.processors.clucore

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.clu.CluProcessor

class CluCoreProcessor(config: Config = ConfigFactory.load("cluprocessor"))
  extends CluProcessor(config) {

  // TODO: ner merge, SRL ner labels, doc date, tests
}
