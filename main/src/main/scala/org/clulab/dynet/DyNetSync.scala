package org.clulab.dynet

import edu.cmu.dynet.ComputationGraph

/** Use this object for synchronized statements in DyNet */
object DyNetSync {

  def withComputationGraph[T](f: => T): T = {
    this.synchronized {
      try {
        f
      }
      finally {
        // Make sure the nodes are freed immediately.  This prevents live object
        // from being trashed and may help prevent memory fragmentation.
        ComputationGraph.clear()
        // Wait for the rest to disappear during finalization which need not be synchronized.
        ComputationGraph.renew()
      }
    }
  }

  def withoutComputationGraph[T](f: => T): T = {
    this.synchronized {
      try {
        f
      }
      finally {
        // Make sure there is a ComputationGraph now as long as we're synchronized and
        // this typically runs before DyNet can be used.  It is otherwise possible
        // that the first graph is constructed when a model loads, without synchronization.

        // See https://stackoverflow.com/questions/9443137/rules-of-initializing-companion-object-values-in-scala.
        // Note that the value defined by an object definition is instantiated lazily. The new m$cls constructor is
        // evaluated not at the point of the object definition, but is instead evaluated the first time m is
        // dereferenced during execution of the program (which might be never at all). An attempt to dereference m
        // again in the course of evaluation of the constructor leads to a infinite loop or run-time error. Other
        // threads trying to dereferencem while the constructor is being evaluated block until evaluation is complete.

        // This seems to do the trick without referring to any internals.
        // classOf[ComputationGraph] does not compile, so the Java version is used.
        ComputationGraph.getClass
      }
    }
  }
}
