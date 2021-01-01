package org.clulab.dynet

import edu.cmu.dynet.ComputationGraph

/** Use this object for synchronized statements in DyNet */
object DyNetSync {
  protected val debug = false
  protected var expectedVersion = 0L
  protected var count = 0
  protected var inSynchronized = false
  // Allow public query of the read-only version.
  def isSynchronized() = inSynchronized

  def before(message: String, expectedVersion: Long): Int = {
    // It is possible for the same thread to re-enter the synchronized section.  Avoid that!
    assert(!inSynchronized)
    inSynchronized = true
    val startCount = count
    count += 1 // Something else will see a different count now.
    if (debug) {
      val threadId = Thread.currentThread.getId
      println(s"Synchronize\t$startCount\tstart\t$threadId\t$message")
    }
    require(ComputationGraph.version == expectedVersion)

    startCount
  }

  def after(startCount: Int, message: String): Unit = {
    require(ComputationGraph.version == expectedVersion)
    if (debug) {
      val threadId = Thread.currentThread.getId // This had better be the original threadId.
      println(s"Synchronize\t$startCount\tstop\t$threadId\t$message")
    }
    val stopCount = count
    assert(startCount + 1 == stopCount)
  }

  def withComputationGraph[T](message: String)(f: => T): T = {
    // In parallel version, synchronize on Thread.currentThread.
    this.synchronized {
      val startCount = before(message, expectedVersion)
      try {
        val result = f // This needs to make all the nodes
        result
      }
      catch {
        case throwable: Throwable =>
          throwable.printStackTrace()
          throw throwable
      }
      finally {
        after(startCount, message)
        // Make sure the nodes are freed immediately with clear().  This prevents live object
        // from being trashed and may help prevent memory fragmentation.
        // However, the line is redundant because ComputationGraph.renew() calls
        // delete immediately and there is no wait for garbage collection.
        // ComputationGraph.clear()
        // Wait for the rest to disappear during finalization which need not be synchronized.
        ComputationGraph.renew()
        expectedVersion += 1
        inSynchronized = false
      }
    }
  }

  def withoutComputationGraph[T](message: String)(f: => T): T = {
    // In parallel version, synchronize on Thread.currentThread.
    this.synchronized {
      // The expectedVersion may not be 0 if initialization was performed previously.
      // This version checking will itself bring in the computation graph in finally.
      val startCount = before(message, ComputationGraph.version)
      try {
        val result = f
        result
      }
      catch {
        case throwable: Throwable =>
          throwable.printStackTrace()
          throw throwable
      }
      finally {
        after(startCount, message)
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
        inSynchronized = false
      }
    }
  }
}
