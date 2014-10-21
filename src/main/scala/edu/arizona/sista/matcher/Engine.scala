package edu.arizona.sista.matcher

import scala.util.control.Breaks._
import scala.collection.mutable.HashMap
import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.matcher.dependencies.DependencyExtractor
import ExtractorEngine.mkExtractor

class ExtractorEngine(val spec: String) {
  // extractors defined in the spec, separated by at least one blank line
  val extractors = spec split """(?m)^\s*$""" map (_.trim) filter (_ != "") map mkExtractor

  // the minimum number of iterations required for every rule to run at least once
  val minIterations = extractors.map(_.startsAt).max

  def extractFrom(sentence: Sentence) = {
    breakable {
      for (iter <- Stream.from(1)) {
        for (extractor <- extractors if extractor.priority matches iter) {
          // do something
        }
      }
    }
  }
}

object ExtractorEngine {
  type ExtractorBuilder = String => Extractor

  // registered extractors go here
  private val registeredExtractors = new HashMap[String, ExtractorBuilder]

  // our extractor is the default
  val defaultExtractorType = "arizona"
  register(defaultExtractorType, DependencyExtractor.apply)

  // register extractors to be used in our rules
  def register(extractorType: String, extractorBuilder: ExtractorBuilder) {
    registeredExtractors += (extractorType -> extractorBuilder)
  }

  // regex to extract fields from rule
  private val field = """(?s)(\w+)\s*:\s*(\w+|\{\{.*\}\})""".r

  def mkExtractor(spec: String): NamedExtractor = {
    val it = for (field(name, value) <- field findAllIn spec) yield (name -> value)
    val fields = Map(it.toSeq: _*)
    val name = fields("name")
    val priority = Priority(fields("priority"))
    val extractorType = fields.getOrElse("type", defaultExtractorType)
    val pattern = fields("pattern").drop(2).dropRight(2)
    val extractor = registeredExtractors(extractorType)(pattern)
    new NamedExtractor(name, priority, extractor)
  }
}

class NamedExtractor(val name: String, val priority: Priority, val extractor: Extractor) extends Extractor {
  def findAllIn(sentence: Sentence): Seq[Map[String, Seq[Int]]] = extractor findAllIn sentence

  def startsAt = priority match {
    case ExactPriority(i) => i
    case IntervalPriority(start, end) => start
    case FromPriority(from) => from
  }
}
