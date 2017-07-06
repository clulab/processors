package org.clulab.processors.coserver

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import org.scalatest.{ Matchers, FlatSpec }

/**
  * Tests of the ProcessorCoreServer.
  *   Written by: Tom Hicks. 6/14/2017.
  *   Last Modified: Update config section name.
  */
class TestProcessorCoreServer extends FlatSpec with Matchers with LazyLogging {

  // load application configuration from the configuration file
  val config = ConfigFactory.load().getConfig("ProcessorCoreServer")
  logger.debug(s"(TestProcessorCoreServer): config=${config}")

  // create a processor core server instance
  val server = new ProcessorCoreServer(config)
  logger.debug(s"(TestProcessorCoreServer): server=${server}")

  "ProcessorCoreServer" should "return actor path to the processor actor pool" in {
    val path = server.getPath
    logger.debug(s"PATH=${path}")
    (path) should not be (null)
    (path.toString) should equal("akka://proc-core-server/user/proc-actor-pool")
  }

}
