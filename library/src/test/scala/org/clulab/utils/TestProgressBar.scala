package org.clulab.utils

class TestProgressBar extends Test {

  behavior of "ProgressBar"

  it should "progress" in {
    ProgressBar(s"Progress", Range(0, 10)).foreach { _ =>
      Thread.sleep(1000)
    }
  }

  it should "show extra messages" in {
    val progressBar = ProgressBar(s"Progress", Range(0, 11))

    progressBar.foreach { index =>
      Thread.sleep(1000)
      progressBar.setExtraMessage(s"at $index")
    }
  }
}
