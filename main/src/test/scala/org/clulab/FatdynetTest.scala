package org.clulab

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FatdynetTest extends FlatSpec with Matchers with BeforeAndAfterAll {
  import org.clulab.fatdynet.utils.Utils

  override def beforeAll(): Unit = {
    Utils.startup()
  }

  override def afterAll(): Unit = {
    Utils.shutdown()
  }
}
