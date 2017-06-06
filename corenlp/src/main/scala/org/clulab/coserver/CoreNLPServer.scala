package org.clulab.coserver

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
// import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }


/**
  * Application to wrap and server various processor capabilities.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Initial creation.
  */
object CoreNLPServer extends App {

  // save any command line arguments
  val argsList = args.toList

  // load application configuration from the configuration file
  val config = ConfigFactory.load()

  // fire up the actor system
  val system = ActorSystem("core-server")

}
