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
  * Application to wrap and serve various processor capabilities.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Set supervisory strategy on router. Redo instance creation.
  */
object ProcessorCoreServer extends App with LazyLogging {

  // save any command line arguments
  private val argsList = args.toList

  // THE instance of the the processor core server
  private var _server: ProcessorCoreServer = _

  /** Return an actor path to the current instance of the processor pool. */
  def getPath: ActorPath = instance

  /** Create a single instance of the processor core server, only if it has been created. */
  def instance: ActorPath = {
    logger.debug(s"(ProcessorCoreServer.instance): server = ${_server}")
    if (_server == null) {                  // create server, iff not already created
      val config = ConfigFactory.load().getConfig("ProcessorCoreServer")
      if (config == null)
        throw new RuntimeException("(ProcessorCoreServer.instance): Unable to read configuration from configuration file.")
      logger.debug(s"(ProcessorCoreServer.instance): config=${config}")
      _server = new ProcessorCoreServer(config)
    }
    logger.debug(s"(ProcessorCoreServer.instance): server => ${_server}")
    _server.getPath                         // return actor path to processor pool
  }

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

  // set supervisory strategy property of the pool to handle errors
  private final val restartEachStrategy: SupervisorStrategy =
    OneForOneStrategy() { case _ => Restart }

  // create a pool of processor actors waiting for work
  private val procPool = system.actorOf(
    ProcessorActor.props(processor).withRouter(
      FromConfig.withSupervisorStrategy(restartEachStrategy)),
    "proc-actor-pool")

  logger.debug(s"(ProcessorCoreServer.ctor): procPool=${procPool}")


  /** Returns an actor path to the current instance of the processor pool. */
  val getPath: ActorPath = procPool.path
}
