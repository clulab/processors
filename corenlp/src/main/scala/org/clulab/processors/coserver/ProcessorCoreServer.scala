package org.clulab.processors.coserver

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor._
import akka.actor.SupervisorStrategy._
import akka.routing._
import akka.event.Logging

import org.clulab.processors._
import org.clulab.processors.bionlp._
import org.clulab.processors.corenlp._
import org.clulab.processors.fastnlp._
import org.clulab.processors.shallownlp._

/**
  * Application to wrap and serve various Processors capabilities.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Reset one leftover debug statement.
  */
object ProcessorCoreServer extends App with LazyLogging {

  // save any command line arguments
  private val argsList = args.toList

  // THE instance of the the processor core server
  private var _pcs: ProcessorCoreServer = _

  /** Create a single instance of the processor core server, only if it has been created. */
  def instance: ProcessorCoreServer = {
    logger.debug(s"(ProcessorCoreServer.instance): pcs = ${_pcs}")
    if (_pcs == null) {                     // create server, iff not already created
      val config = ConfigFactory.load().getConfig("ProcessorCoreServer")
      if (config == null)
        throw new RuntimeException("(ProcessorCoreServer.instance): Unable to read configuration from configuration file.")
      logger.debug(s"(ProcessorCoreServer.instance): config=${config}")
      _pcs = new ProcessorCoreServer(config)
    }
    logger.debug(s"(ProcessorCoreServer.instance): pcs => ${_pcs}")
    _pcs
  }

  /** Return an actor ref to the current instance of the router. */
  def router: ActorRef = instance.router
}


class ProcessorCoreServer (

  /** Application-specific portion of the configuration file. */
  val config: Config

) extends LazyLogging {

  if (config == null)
    throw new RuntimeException("(ProcessorCoreServer.ctor): Empty configuration argument not allowed.")

  // create the Processor engine specified by the configuration and used by this server
  private val processor: Processor = {
    val proc = if (config.hasPath("server.processor")) config.getString("server.processor")
               else "core"
    proc.toLowerCase match {
      case "bio" => new BioNLPProcessor(removeFigTabReferences = true)
      case "core" => new CoreNLPProcessor()
      case "fast" => new FastNLPProcessor(useMalt = false)
      case "fastbio" => new FastBioNLPProcessor(removeFigTabReferences = true)
      case _ => new ShallowNLPProcessor()
    }
  }
  logger.debug(s"(ProcessorCoreServer.ctor): processor=${processor}")

  // fire up the actor system
  private val system = ActorSystem("proc-core-server", config)

  logger.debug(s"(ProcessorCoreServer.ctor): system=${system}")

  // create supervisory strategy for the router to handle errors
  private final val restartEachStrategy: SupervisorStrategy =
    OneForOneStrategy() { case _ => Restart }

  // create a router to a pool of processor actors waiting for work
  private val procPool: ActorRef = system.actorOf(
    ProcessorActor.props(processor).withRouter(
      FromConfig.withSupervisorStrategy(restartEachStrategy)),
    "proc-actor-pool")

  logger.debug(s"(ProcessorCoreServer.ctor): procPool=${procPool}")

  /** Returns an actor ref to the internal instance of the pooled router. */
  val router: ActorRef = procPool
}
