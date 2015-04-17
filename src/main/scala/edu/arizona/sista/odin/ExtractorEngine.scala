package edu.arizona.sista.odin

import scala.reflect.ClassTag
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin.impl.{ RuleReader, Extractor }

class ExtractorEngine(val extractors: Seq[Extractor], val globalAction: Action) {
  // the minimum number of iterations required for every rule to run at least once
  val minIterations = extractors.map(_.startsAt).max

  /** Extract mentions from a document.
   *
   *  @param document a processor's document
   *  @param initialState (optional) a state instance that may be prepopulated
   *  @return a sequence of mentions extracted from the document
   */
  def extractFrom(document: Document, initialState: State = new State): Seq[Mention] = {
    @annotation.tailrec
    def loop(i: Int, state: State): Seq[Mention] = extract(i, state) match {
      case Nil if i >= minIterations => state.allMentions  // we are done
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
      // apply globalAction to all extracted mentions
      val finalMentions = globalAction(extractedMentions, state)
      // only return mentions that are not already in the state
      finalMentions filterNot state.contains
    }

    loop(1, initialState)
  }
}

object ExtractorEngine {
  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   *  @param actions an object that contains all the actions used by the rules
   *  @param globalAction (optional) an action that will be applied to the extracted
   *                      mentions at the end of each iteration
   */
  def apply[A <: Actions : ClassTag](
    rules: String,
    actions: A = new Actions,
    globalAction: Action = identityAction
  ): ExtractorEngine = {
    val reader = new RuleReader(actions)
    val extractors = reader.read(rules)
    new ExtractorEngine(extractors, globalAction)
  }
}
