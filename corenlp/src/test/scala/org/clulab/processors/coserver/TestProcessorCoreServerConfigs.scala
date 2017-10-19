package org.clulab.processors.coserver

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

import org.scalatest.{ Matchers, FlatSpec }

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import org.clulab.processors._
import org.clulab.processors.bionlp._
import org.clulab.processors.corenlp._
import org.clulab.processors.fastnlp._
import org.clulab.processors.shallownlp._

/**
  * Tests of the ProcessorCoreServer.
  *   Written by: Tom Hicks. 7/12/2017.
  *   Last Modified: Shutdown each server after testing it.
  */
class TestProcessorCoreServerConfigs extends FlatSpec with Matchers with LazyLogging {

  // load application configuration from the configuration file
  val config = ConfigFactory.load().getConfig("ProcessorCoreServer")

  val prefix = "server.processor"   // prefix string for all PCS config values

  "ProcessorCoreServer configuration" should "should load all parameters" in {
    val prefix = "server.processor"
    (config) should not be (null)
    (config.hasPath(s"${prefix}.type")) should be (true)
    (config.hasPath(s"${prefix}.internStrings")) should be (true)
    (config.hasPath(s"${prefix}.maxSentenceLength")) should be (true)
    (config.hasPath(s"${prefix}.removeFigTabReferences")) should be (true)
    (config.hasPath(s"${prefix}.removeBibReferences")) should be (true)
    (config.hasPath(s"${prefix}.withChunks")) should be (true)
    (config.hasPath(s"${prefix}.withContext")) should be (true)
    (config.hasPath(s"${prefix}.withCRFNER")) should be (true)
    (config.hasPath(s"${prefix}.withRuleNER")) should be (true)
    (config.hasPath(s"${prefix}.withDiscourse")) should be (true)
  }

  "Default configuration" should "should instantiate a CoreNLPProcessor" in {
    val pcs = new ProcessorCoreServer(config)
    (pcs) should not be (null)
    (pcs.router) should not be (null)
    (pcs.processor) should not be (null)
    (pcs.processor.isInstanceOf[CoreNLPProcessor]) should be (true)
    Await.result(pcs.system.terminate(), 10.seconds)
  }

  "Bogus processor type" should "should instantiate a ShallowNLPProcessor" in {
    val modConfig = config.withValue(s"${prefix}.type", ConfigValueFactory.fromAnyRef("BAD"))
    val pcs = new ProcessorCoreServer(modConfig)
    (pcs) should not be (null)
    (pcs.router) should not be (null)
    (pcs.processor) should not be (null)
    (pcs.processor.isInstanceOf[ShallowNLPProcessor]) should be (true)
    Await.result(pcs.system.terminate(), 10.seconds)
  }

  "Bio processor type" should "should instantiate a BioNLPProcessor" in {
    val modConfig = config.withValue(s"${prefix}.type", ConfigValueFactory.fromAnyRef("bio"))
    val pcs = new ProcessorCoreServer(modConfig)
    (pcs) should not be (null)
    (pcs.router) should not be (null)
    (pcs.processor) should not be (null)
    (pcs.processor.isInstanceOf[BioNLPProcessor]) should be (true)
    Await.result(pcs.system.terminate(), 10.seconds)
  }

  "Fast processor type" should "should instantiate a FastNLPProcessor" in {
    val modConfig = config.withValue(s"${prefix}.type", ConfigValueFactory.fromAnyRef("fast"))
    val pcs = new ProcessorCoreServer(modConfig)
    (pcs) should not be (null)
    (pcs.router) should not be (null)
    (pcs.processor) should not be (null)
    (pcs.processor.isInstanceOf[FastNLPProcessor]) should be (true)
    Await.result(pcs.system.terminate(), 10.seconds)
  }

  "FastBio processor type" should "should instantiate a FastBioNLPProcessor" in {
    val modConfig = config.withValue(s"${prefix}.type", ConfigValueFactory.fromAnyRef("fastbio"))
    val pcs = new ProcessorCoreServer(modConfig)
    (pcs) should not be (null)
    (pcs.router) should not be (null)
    (pcs.processor) should not be (null)
    (pcs.processor.isInstanceOf[FastBioNLPProcessor]) should be (true)
    Await.result(pcs.system.terminate(), 10.seconds)
  }

}
