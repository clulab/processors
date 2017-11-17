package org.clulab.openie


import org.clulab.openie.entities.RuleBasedEntityFinder
import org.clulab.processors.Document
import org.json4s.jackson.JsonMethods._
import org.clulab.serialization.json.JSONSerializer


object IETestUtils {

  val entityFinder: RuleBasedEntityFinder = RuleBasedEntityFinder(maxHops = 2)

  def jsonStringToDocument(jsonstr: String): Document = JSONSerializer.toDocument(parse(jsonstr, useBigDecimalForDouble = true))


}