package org.clulab.coserver

import org.slf4j.LoggerFactory
import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }
import akka.event.Logging
import akka.routing.{ FromConfig, RoundRobinPool }

import org.clulab.processors._              // REMOVE LATER
import org.clulab.processors.corenlp._      // REMOVE LATER

import CoreProcessorCommands._              // REMOVE LATER
import CoreProcessorReplies._               // REMOVE LATER

/**
  * Application to wrap and server various processor capabilities.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Build & test initial actor structure.
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

  // send some messages to test the infrastructure (REMOVE LATER):
  procPool ! AnnotateCmd("This sentence should be annotated by a child actor.")
  procPool ! AnnotateCmd("Another sentence to be annotated by a child actor.", false)
  procPool ! AnnotateCmd("This last sentence is to be annotated by a child actor, too.", true)
  Thread.sleep(100)
  system.terminate()

}
