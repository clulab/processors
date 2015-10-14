package edu.arizona.sista.odin

import scala.reflect.ClassTag
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin.impl.{ RuleReader, Extractor }

class ExtractorEngine(val extractors: Vector[Extractor], val globalAction: Action) {

  // minimum number of iterations required to satisfy the priorities
  // of all extractors
  val minIterations = extractors.map(_.priority.minIterations).max

  /** Extract mentions from a document.
   *
   *  @param doc a processor's document
   *  @return a sequence of mentions extracted from the document
   */
  def extractFrom(doc: Document): Seq[Mention] = extractFrom(doc, new State)

  /** Extract mentions from a document.
   *
   *  @param document a processor's document
   *  @param initialState a state instance that may be prepopulated
   *  @return a sequence of mentions extracted from the document
   */
  def extractFrom(document: Document, initialState: State): Seq[Mention] = {
    @annotation.tailrec
    def loop(i: Int, state: State): Seq[Mention] = extract(i, state) match {
      case Nil if i >= minIterations => state.allMentions // we are done
      case Nil => loop(i + 1, state)
      case mentions => loop(i + 1, state.updated(mentions))
    }

    def extract(i: Int, state: State): Seq[Mention] = {
      // extract mentions using extractors (each extractor applies its own action)
      val extractedMentions = for {
        extractor <- extractors
        if extractor.priority matches i
        mention <- extractor.findAllIn(document, state)
      } yield mention
      // apply globalAction and filter resulting mentions
      val finalMentions = for {
        mention <- globalAction(extractedMentions, state)
        if mention.isValid && !state.contains(mention)
      } yield mention
      // return the final mentions
      finalMentions
    }

    loop(1, initialState)
  }

  def extractByType[M <: Mention : ClassTag](
      document: Document,
      initialState: State = new State
  ): Seq[M] =
    extractFrom(document, initialState) flatMap {
      case m: M => Some(m)
      case _ => None
    }

}

object ExtractorEngine {

  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   *  @param actions an object that contains all the actions used by the rules
   *  @param globalAction an action that will be applied to the extracted
   *                      mentions at the end of each iteration
   */
  def apply(rules: String, actions: Actions, globalAction: Action): ExtractorEngine = {
    val reader = new RuleReader(actions)
    val extractors = reader.read(rules)
    new ExtractorEngine(extractors, globalAction)
  }

  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   */
  def apply(rules: String): ExtractorEngine =
    apply(rules, new Actions, identityAction)

  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   *  @param actions an object that contains all the actions used by the rules
   */
  def apply(rules: String, actions: Actions): ExtractorEngine =
    apply(rules, actions, identityAction)

  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   *  @param globalAction an action that will be applied to the extracted
   *                      mentions at the end of each iteration
   */
  def apply(rules: String, globalAction: Action): ExtractorEngine =
    apply(rules, new Actions, globalAction)

}
