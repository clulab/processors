package edu.arizona.sista.matcher

import scala.collection.mutable.HashMap
import edu.arizona.sista.processors.{Document, Sentence}
import edu.arizona.sista.matcher.dependencies.DependencyExtractor
import NamedEntityExtractor.getEntityMentions

class ExtractorEngine(val spec: String, val actions: AnyRef) {
  // invokes actions through reflection
  val mirror = new ActionMirror(actions)

  val extractors = parseSpec(spec)

  // the minimum number of iterations required for every rule to run at least once
  val minIterations = extractors.map(_.startsAt).max

  def extractFrom(document: Document) = {
    val state = new State(document)
    state.update(getEntityMentions(document))

    var updated = true
    var iter = 0

    while (updated || iter < minIterations) {
      iter += 1
      updated = false
      for (extractor <- extractors if extractor.priority matches iter) {
        val mentions = extractor.extractFrom(document, state)
        if (mentions.nonEmpty) {
          state.update(mentions)
          updated = true
        }
      }
    }

    state.allMentions
  }

  def parseSpec(spec: String): Seq[NamedExtractor] = {
    // extractors defined in the spec, separated by at least one blank line
    val extractors = spec split """(?m)^\s*$""" map (_.trim) filter (_ != "") map mkExtractor
    val names = extractors map (_.name)
    require(names.size == names.distinct.size, "rule names should be unique")
    extractors
  }

  def mkExtractor(spec: String): NamedExtractor = {
    val fieldPat = ExtractorEngine.fieldPattern
    val it = for (fieldPat(name, value) <- fieldPat findAllIn spec) yield (name -> value)
    val fields = Map(it.toSeq: _*)
    val name = fields("name")
    val priority = Priority(fields.getOrElse("priority", ExtractorEngine.defaultPriority))
    val action = mirror.reflect(fields("action"))
    val extractorType = fields.getOrElse("type", ExtractorEngine.defaultExtractorType)
		val pattern = fields("pattern").drop(2).dropRight(2)
    val extractor = ExtractorEngine.registeredExtractors(extractorType)(pattern)
    new NamedExtractor(name, priority, extractor, action)
  }
}

object ExtractorEngine {
  type ExtractorBuilder = String => Extractor

  // registered extractors go here
  private val registeredExtractors = new HashMap[String, ExtractorBuilder]

  val defaultExtractorType = "arizona"
  val defaultPriority = "1+"

  // our extractor is registered by default
  register(defaultExtractorType, DependencyExtractor.apply)

  // register extractors to be used in our rules
  def register(extractorType: String, extractorBuilder: ExtractorBuilder) {
    registeredExtractors += (extractorType -> extractorBuilder)
  }

  // regex to extract fields from rule
  private val fieldPattern = """(?s)(\w+)\s*:\s*(\w+|\{\{.*\}\})""".r
}
