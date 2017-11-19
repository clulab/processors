package org.clulab.processors.server

import com.typesafe.scalalogging.LazyLogging

import akka.actor._
import akka.event.Logging

/**
  * Class to shutdown the actor system when the router dies.
  *   Written by: Tom Hicks. 11/10/2017.
  *   Last Modified: Correct props method docs.
  */
class ServerDeathWatchActor (

  /** The actor system shared between watcher and watched. */
  system: ActorSystem,

  /** The instance of the router to keep watch on. */
  router: ActorRef

) extends Actor {

  val log = Logging(context.system, this)

  context.watch(router)                     // initiate the death watch

  def receive = {                           // wait for word of routers death
    case Terminated(corpse) =>
      if (corpse == router) {
        system.terminate()
      }
  }
}


object ServerDeathWatchActor {
  /**
    * Constructor to create Props for an actor of this type.
    *   @param system The actor system which created the router to be watched.
    *   @param router The instance of the router to keep watch on.
    *   @return a Props for creating this actor.
    */
  def props (system: ActorSystem, router: ActorRef): Props =
    Props(new ServerDeathWatchActor(system, router))
}
