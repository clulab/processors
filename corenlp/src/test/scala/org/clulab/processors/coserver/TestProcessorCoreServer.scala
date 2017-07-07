package org.clulab.processors.coserver

import org.scalatest.{ Matchers, FlatSpec }

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor._

/**
  * Tests of the ProcessorCoreServer.
  *   Written by: Tom Hicks. 6/14/2017.
  *   Last Modified: Add additional small test of PCS class.
  */
class TestProcessorCoreServer extends FlatSpec with Matchers with LazyLogging {

  private val system = ActorSystem("proc-core-server")

  // load application configuration from the configuration file
  val config = ConfigFactory.load().getConfig("ProcessorCoreServer")
  logger.debug(s"config=${config}")

  // create a processor core server instance
  val pcs = new ProcessorCoreServer(config)
  logger.debug(s"ProcessorCoreServer instance=${pcs}")

  "ProcessorCoreServer" should "return actor path to the processor actor pool" in {
    val path = pcs.getPath
    logger.debug(s"PATH=${path}")
    (path) should not be (null)
    (path.toString) should equal("akka://proc-core-server/user/proc-actor-pool")
  }

  // select the processor actor pool
  val server: ActorSelection = system.actorSelection(pcs.getPath)
  logger.debug(s"Found server ref: ${server}")

  it should "return the processor actor pool" in {
    (server) should not be (null)
  }

}
