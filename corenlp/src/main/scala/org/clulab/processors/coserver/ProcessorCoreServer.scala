package org.clulab.processors.coserver

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }
import akka.event.Logging
import akka.routing.FromConfig

import org.clulab.processors._
import org.clulab.processors.bionlp._
import org.clulab.processors.corenlp._
import org.clulab.processors.fastnlp._
import org.clulab.processors.shallownlp._

/**
  * Application to wrap and serve various processor capabilities.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Select/instantiate Processor from config.
  */
object ProcessorCoreServer extends App with LazyLogging {

  // save any command line arguments
  private val argsList = args.toList

  // load application configuration from the configuration file
  private val config = ConfigFactory.load().getConfig("ProcessorCoreService")

  // create the Processor engine specified by the configuration and used by this server
  private val processor: Processor = {
    val proc = config.getString("server.processor")
    proc.toLowerCase match {
      case "bio" => new BioNLPProcessor(removeFigTabReferences = true)
      case "core" => new CoreNLPProcessor()
      case "fast" => new FastNLPProcessor(useMalt = false)
      case "fastbio" => new FastBioNLPProcessor(removeFigTabReferences = true)
      case _ => new ShallowNLPProcessor()
    }
  }

  // fire up the actor system
  private val system = ActorSystem("proc-core-server", config)

  // create a pool of processor actors waiting for work
  private val procPool = system.actorOf(
    FromConfig.props(ProcessorActor.props(processor)), "proc-actor-pool")

  // return an actor path to the current instance of the processor pool. */
  def getInstance = procPool.path

}
