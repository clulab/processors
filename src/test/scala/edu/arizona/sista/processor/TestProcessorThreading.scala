package edu.arizona.sista.processor

import org.scalatest.junit.AssertionsForJUnit
import org.junit.{Before, Test}
import edu.arizona.sista.processor.corenlp.CoreNLPProcessor
import scala.io.Source
import java.util.concurrent.{TimeUnit, Executors}
import java.lang.Long
import junit.framework.Assert
import edu.arizona.sista.processor.fastnlp.FastNLPProcessor

/**
 * Tests that CoreNLPProcessor (and other processors) work in multi-threading mode
 * User: mihais
 * Date: 1/4/14
 */
class TestProcessorThreading extends AssertionsForJUnit {
  var corenlp:Processor = null
  var fastnlp:Processor = null

  @Before def constructProcessor() {
    corenlp = new CoreNLPProcessor(internStrings = true)
    fastnlp = new FastNLPProcessor(internStrings = true)
  }

  @Test def testTwoThreadsSameProcCoreNLP() {
    runTwoThreads(List(corenlp, corenlp, corenlp).toArray)
  }

  @Test def testTwoThreadsDifferentProcCoreNLP() {
    runTwoThreads(List(corenlp, new CoreNLPProcessor(), new CoreNLPProcessor()).toArray)
  }

  @Test def testTwoThreadsSameProcFastNLP() {
    runTwoThreads(List(fastnlp, fastnlp, fastnlp).toArray)
  }

  @Test def testTwoThreadsDifferentProcFastNLP() {
    runTwoThreads(List(fastnlp, new FastNLPProcessor(), new FastNLPProcessor()).toArray)
  }

  private def runTwoThreads(procs:Array[Processor]) {
    val text = Source.fromFile("src/main/resources/edu/arizona/sista/processor/raw_text.txt").getLines.mkString(" ")
    println(s"Read a text with ${text.length} characters:\n${text}")
    // run the annotation pipeline once to load all models in memory
    procs(0).annotate("This is a simple sentence.")

    // now measure actual ellapsed time
    val startTime = System.currentTimeMillis()
    procs(0).annotate(text)
    val estimatedSeqTime = System.currentTimeMillis() - startTime
    println(s"Sequential time: $estimatedSeqTime ms")

    // now annotate the same text in two threads
    val noThreads = procs.length - 1
    val estimatedTimes = new Array[Double](noThreads)
    val executor = Executors.newFixedThreadPool(2) // we assume any machine nowadays has at least two cores
    for(i <- 0 until noThreads) {
      val worker = new MyThread(estimatedTimes, i, procs(i + 1), text)
      executor.execute(worker)
    }
    executor.shutdown()
    executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS)
    println("Finished all threads.")

    println("Estimated thread times:")
    for(i <- 0 until noThreads) {
      println(s"Thread #$i: " + estimatedTimes(i))
      // estimated times should not be too slow compared with the sequential one
      Assert.assertTrue(estimatedTimes(i) < estimatedSeqTime * 1.5)
    }
  }
}

class MyThread (val estimatedTimes:Array[Double],
                val index:Int,
                val proc:Processor,
                val text:String) extends Runnable  {
  override def run() {
    // run the annotation pipeline once to load all models in memory
    proc.annotate("This is a simple sentence.")

    // the actual job
    val startTime = System.currentTimeMillis()
    proc.annotate(text)
    estimatedTimes(index) = System.currentTimeMillis() - startTime
  }
}
