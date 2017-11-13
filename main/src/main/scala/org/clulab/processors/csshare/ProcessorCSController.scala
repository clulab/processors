package org.clulab.processors.csshare

/**
  * Common interface for implementations which rely on a client/server paradigmn and
  * need to control the operation of the client and/or server. Default NOP implementations
  * can be overridden by actual client/server implementations, when needed.
  *   Written by: Tom Hicks. 11/13/2017.
  *   Last Modified: Initial creation by refactoring.
  */
trait ProcessorCSController {

  /** Shutdown the current instance of the client, if any. */
  def shutdownClient: Unit = { /** default NOP can be overridden, if needed for C/S. */ }

  /** Shutdown the remote processor server AND the current instance of the client. */
  def shutdownClientServer: Unit = { /** default NOP can be overridden, if needed for C/S. */ }

  /** Send the server a message to shutdown actors and terminate the server. */
  def shutdownServer: Unit = { /** default NOP can be overridden, if needed for C/S. */ }

}
