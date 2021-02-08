package org.clulab.processors

import org.clulab.dynet.{ConstEmbeddingsGlove, Utils => DynetUtils}
import org.clulab.fatdynet.utils.Utils
import org.clulab.processors.clu.CluProcessor
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

class FatdynetTest extends FlatSpec with Matchers with BeforeAndAfterAll {
  // We can't really be debugging without the debug build of dynet.  However,
  // there may as well be a way to activate the shutdown process for when a
  // human is observing the testing.
  val debugging = System.getenv.containsKey("MALLOC_TRACE")
  var proc: CluProcessor = null // sorry

  override def beforeAll(): Unit = {
    Utils.startup()
    proc = newCluProcessor()
  }

  override def afterAll(): Unit = {
    // Debugging can sometimes cause a crash depending on garbage collection performance.
    // Its use should be limited to times when a human is watching over it.
    if (debugging) {
      stop()
      Utils.shutdown()
    }
  }

  def newCluProcessor(): CluProcessor = {
    DynetUtils.initializeDyNet()
    new CluProcessor()
  }

  def stop(): Unit = {
    // This is for debugging memory leaks.  It will cause other tests to crash.
    proc = null
    ConstEmbeddingsGlove.SINGLETON = null
  }
}
