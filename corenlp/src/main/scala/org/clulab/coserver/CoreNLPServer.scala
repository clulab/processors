package org.clulab.coserver

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }
import akka.event.Logging
import akka.routing.FromConfig

import org.clulab.processors._
import org.clulab.processors.corenlp._

/**
  * Application to wrap and server various processor capabilities.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Move tests to test file. Cleanup.
  */
object CoreNLPServer extends App with LazyLogging {

  // save any command line arguments
  val argsList = args.toList

  // load application configuration from the configuration file
  val config = ConfigFactory.load()

  // create a new faster dependency parser
  val core: Processor = new CoreNLPProcessor() // this uses the slower constituent parser

  // fire up the actor system
  val system = ActorSystem("core-server")

  // create a pool of processor actors waiting for work
  val procPool = system.actorOf(FromConfig.props(ProcessorActor.props(core)), "proc-actor-pool")

}
