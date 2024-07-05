package org.clulab.processors

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.struct.TrueEntityValidator
import org.clulab.utils.Test
import org.scalatest.BeforeAndAfterAll

class CluTest extends Test with BeforeAndAfterAll {
  var proc: BalaurProcessor = null // sorry
  val debugging = false

  override def beforeAll(): Unit = {
    proc = newProcessor()
  }

  override def afterAll(): Unit = {
    // Debugging can sometimes cause a crash depending on garbage collection performance.
    // Its use should be limited to times when a human is watching over it.
    if (debugging) {
      stop()
    }
  }

  def newProcessor(): BalaurProcessor = {
    // A custom NER to make sure this works
    val kbs = Seq(
      "org/clulab/processors/D.tsv"
    )
    val lexiconNer = LexiconNER(kbs, Seq(false), useLemmasForMatching = false) // case sensitive match on this KB

    new BalaurProcessor(optionalNER = Some(lexiconNer))
  }

  def stop(): Unit = {
    // This is for debugging memory leaks.  It will cause other tests to crash.
    proc = null
  }
}
