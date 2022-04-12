package org.clulab.processors

import org.clulab.struct.DirectedGraphEdgeIterator
import org.scalatest._
import org.clulab.processors.corenlp.CoreNLPProcessor
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import java.util.concurrent.{TimeUnit, Executors}
import java.lang.Long
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.TestUtils
import TestProcessorThreading._

/**
 * Tests that CoreNLPProcessor (and other processors) work in multi-threading mode
 * User: mihais
 * Date: 1/4/14
 */
class TestProcessorThreading extends FlatSpec with Matchers {
  var corenlp:Processor = new CoreNLPProcessor(internStrings = true)
  var fastnlp:Processor = new FastNLPProcessor(internStrings = true)

  "2 identical FastNLPProcessors" should "run Ok in 2 threads" in {
    val (times, outputs) = runTwoThreads(List(fastnlp, fastnlp, fastnlp).toArray)
    times.length should be (3)
    outputs.length should be (3)

    // the threaded times should not be too slow compared with the sequential one
    for(i <- 1 until times.length) {
      (times(i) < times(0) * 10) should be (true)
    }
    // make sure each thread produced the same output as the sequential job
    for(i <- 1 until outputs.length) {
      outputs(i) should be (outputs(0))
    }
  }

  "2 identical CoreNLPProcessors" should "run Ok in 2 threads" in {
    val (times, outputs) = runTwoThreads(List(corenlp, corenlp, corenlp).toArray)
    times.length should be (3)
    outputs.length should be (3)

    // the threaded times should not be too slow compared with the sequential one
    for(i <- 1 until times.length) {
      (times(i) < times(0) * 10) should be (true)
    }
    // make sure each thread produced the same output as the sequential job
    for(i <- 1 until outputs.length) {
      outputs(i) should be (outputs(0))
    }
  }

  "2 different FastNLPProcessors" should "run Ok in 2 threads" in {
    val (times, outputs) = runTwoThreads(List(fastnlp, new FastNLPProcessor(), new FastNLPProcessor()).toArray)
    times.length should be (3)
    outputs.length should be (3)

    // the threaded times should not be too slow compared with the sequential one
    for(i <- 1 until times.length) {
      (times(i) < times(0) * 10) should be (true)
    }
    // make sure each thread produced the same output as the sequential job
    for(i <- 1 until outputs.length) {
      outputs(i) should be (outputs(0))
    }
  }

  "2 different CoreNLPProcessors" should "run Ok in 2 threads" in {
    val (times, outputs) = runTwoThreads(List(corenlp, new CoreNLPProcessor(), new CoreNLPProcessor()).toArray)
    times.length should be (3)
    outputs.length should be (3)

    // the threaded times should not be too slow compared with the sequential one
    for(i <- 1 until times.length) {
      (times(i) < times(0) * 10) should be (true)
    }
    // make sure each thread produced the same output as the sequential job
    for(i <- 1 until outputs.length) {
      outputs(i) should be (outputs(0))
    }
  }

  private def runTwoThreads(procs:Array[Processor]):(Array[Long], Array[String]) = {
    // estimated times; position 0 is the sequential run
    val estimatedTimes = new ListBuffer[Long]
    // produced dependencies; position 0 is the output of the sequential run
    val outputs = new ListBuffer[String]()

    val text = TestUtils.readFile("org/clulab/processors/raw_text.txt")
    //println(s"Read a text with ${text.length} characters:\n${text}")
    // run the annotation pipeline once to load all models in memory
    procs(0).annotate("This is a simple sentence.")

    // now measure actual ellapsed time and output in the sequential run
    val startTime = System.currentTimeMillis()
    val doc = procs(0).annotate(text)
    estimatedTimes += (System.currentTimeMillis() - startTime)
    outputs += getDependencies(doc)

    // now annotate the same text using multiple threads on two cores
    val noThreads = procs.length - 1
    val estimatedThreadTimes = new Array[Long](noThreads)
    val dependencies = new Array[String](noThreads)
    val executor = Executors.newFixedThreadPool(2) // we assume any machine nowadays has at least two cores
    for(i <- 0 until noThreads) {
      val worker = new MyThread(estimatedThreadTimes, i, procs(i + 1), text, dependencies)
      executor.execute(worker)
    }
    executor.shutdown()
    executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS)
    //println("Finished all threads.")
    estimatedTimes ++= estimatedThreadTimes
    outputs ++= dependencies

    //println("All estimated time: " + estimatedTimes)
    (estimatedTimes.toArray, outputs.toArray)
  }
}

object TestProcessorThreading {
  def getDependencies(doc:Document):String = {
    val os = new StringBuilder
    for(s <- doc.sentences) {
      assert(s.dependencies.isDefined)
      val it = new DirectedGraphEdgeIterator[String](s.dependencies.get)
      while(it.hasNext) {
        val d = it.next()
        os.append(s"${d._1} ${d._2} ${d._3}\n")
      }
    }
    os.toString()
  }
}

class MyThread (val estimatedTimes:Array[Long],
                val index:Int,
                val proc:Processor,
                val text:String,
                val dependencies:Array[String]) extends Runnable  {
  override def run() {
    // run the annotation pipeline once to load all models in memory
    proc.annotate("This is a simple sentence.")

    // the actual job
    val startTime = System.currentTimeMillis()
    val doc = proc.annotate(text)
    estimatedTimes(index) = System.currentTimeMillis() - startTime

    dependencies(index) = getDependencies(doc)
  }
}
