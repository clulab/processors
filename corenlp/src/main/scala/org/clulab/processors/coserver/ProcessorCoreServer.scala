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
  *   Last Modified: Split core server into object/class.
  */
object ProcessorCoreServer extends App with LazyLogging {

  // save any command line arguments
  private val argsList = args.toList

  // load application configuration from the configuration file
  private val config = ConfigFactory.load().getConfig("ProcessorCoreService")

  // create an instance of the server
  private val server = new ProcessorCoreServer(config)

  /** Return an actor path to the current instance of the processor pool. */
  def getPath = server.getPath

}


class ProcessorCoreServer (

  /** Application-specific portion of the configuration file. */
  val config: Config

) extends LazyLogging {

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
  logger.debug(s"(ProcessorCoreServer.Class): processor=${processor}")

  // fire up the actor system
  private val system = ActorSystem("proc-core-server", config)

  logger.debug(s"(ProcessorCoreServer.Class): system=${system}")

  // create a pool of processor actors waiting for work
  private val procPool = system.actorOf(
    FromConfig.props(ProcessorActor.props(processor)), "proc-actor-pool")

  logger.debug(s"(ProcessorCoreServer.Class): procPoll=${procPool}")

  /** Return an actor path to the current instance of the processor pool. */
  def getPath = procPool.path

}
