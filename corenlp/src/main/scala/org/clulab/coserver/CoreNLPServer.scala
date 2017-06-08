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
  *   Last Modified: Hide vals. Add getInstance to return actor ref of pool.
  */
object CoreNLPServer extends App with LazyLogging {

  // save any command line arguments
  private val argsList = args.toList

  // load application configuration from the configuration file
  private val config = ConfigFactory.load()

  // private lazy val processor: Processor = new FastNLPProcessor(useMalt = false)
  // private lazy val processor: Processor = new BioNLPProcessor(removeFigTabReferences = true)
  // private lazy val processor: Processor = new FastBioNLPProcessor(removeFigTabReferences = true)
  // create a slower constituent parser
  private val processor: Processor = new CoreNLPProcessor()

  // fire up the actor system
  private val system = ActorSystem("core-server")

  // create a pool of processor actors waiting for work
  private val procPool = system.actorOf(
    FromConfig.props(ProcessorActor.props(processor)), "proc-actor-pool")

  // return an ActorRef to the current instance of the processor pool. */
  def getInstance: ActorRef = procPool

}
