package org.clulab.ie

import java.util

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.impl.Taxonomy
import org.clulab.odin.{ EventMention, Mention, TextBoundMention }
import org.clulab.struct.DirectedGraph
import org.clulab.utils.DependencyUtils
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor


/**
  * Performs semantic polarity inversion based on event context.
  */
object SemanticPolarity extends LazyLogging {

  // Taxonomy object
  val taxonomy = readTaxonomy("org/clulab/grammar/taxonomy.yml")

  val POS_REG_LABELS = taxonomy.hypernymsFor("PositiveRegulation")
  val NEG_REG_LABELS = taxonomy.hypernymsFor("NegativeRegulation")

  private def readTaxonomy(path: String): Taxonomy = {
    val url = getClass.getClassLoader.getResource(path)
    val source = if (url == null) scala.io.Source.fromFile(path) else scala.io.Source.fromURL(url)
    val input = source.mkString
    source.close()
    val yaml = new Yaml(new Constructor(classOf[util.Collection[Any]]))
    val data = yaml.load(input).asInstanceOf[util.Collection[Any]]
    Taxonomy(data)
  }


  // These are used to detect semantic inversions of polarized events. See semanticNegativeIndices
  val SEMANTIC_NEGATIVE_PATTERN = "(?i)attenu|block|combat|deactiv|decreas|degrad|delet|deplet|diminish|disrupt|dominant-negative|fight|impair|imped|inhibit|knockdown|knockout|limit|loss|lower|negat|reduc|reliev|repress|restrict|revers|silenc|shRNA|siRNA|slow|starv|suppress|supress|target".r

  val MODIFIER_LABELS = "amod".r

  val NOUN_LABELS = "nn".r

  val OF_LABELS = "prep_of".r

  /** Gets a mention. If it is an EventMention with a polarized label
    * and it is semantically negated an odd number of times, returns a new mention
    * with the label flipped. Or else it returns the mention unmodified */
  def analyzeSemanticPolarity(mention: Mention): Mention = mention match {
    // We can only attempt to flip the polarity of ComplexEvents with a trigger
    case em: EventMention if em matches OdinUtils.CAUSAL_RELATION =>
      val trigger = em.trigger
      val arguments = em.arguments.values.flatten
      // get token indices to exclude in the negation search
      // do not exclude args as they may involve regulations
      val excluded = trigger.tokenInterval.toSet
      // count total number of negatives between trigger and each argument
      val numNegatives = arguments.flatMap(arg => semanticNegativeIndices(trigger, arg, excluded)).toSeq.distinct.length
      // does the label need to be flipped?
      numNegatives % 2 != 0 match {
        // odd number of negatives
        case true =>
          val newLabels = flipLabels(em.label)
          // trigger labels should match event labels
          val newTrigger = em.trigger.copy(labels = newLabels)
          // return new mention with flipped label
          em.copy(labels = newLabels, trigger = newTrigger, foundBy = s"${em.foundBy}-after-polarity-flipping")
        // return mention unmodified
        case false => em
      }
    case m => m
  }


  /** gets a polarized label and returns it flipped */
  def flipLabels(label: String): Seq[String] = label match {
    case "PositiveRegulation" => NEG_REG_LABELS
    case "NegativeRegulation" => POS_REG_LABELS
    case "IncreaseEvent" => taxonomy.hypernymsFor("DecreaseEvent")
    case "DecreaseEvent" => taxonomy.hypernymsFor("IncreaseEvent")
    case _ =>
      logger.error(s"Can't flip label '$label'")
      taxonomy.hypernymsFor(label)
  }


  /** Gets a trigger, an argument and a set of tokens to be ignored.
    * Returns the number of semantic negatives found in the shortest possible path
    * between the trigger and the argument.
    */
  def semanticNegativeIndices(trigger: Mention, arg: Mention, excluded: Set[Int]): Seq[Int] = {
    // it is possible for the trigger and the arg to be in different sentences because of coreference
    if (trigger.sentence != arg.sentence) return Nil
    val deps = trigger.sentenceObj.dependencies.get
    // find shortestPath between the trigger head token and the argument head token
    val shortestPath: Option[Seq[Int]] = for {
      triggerHead <- DependencyUtils.findHeadStrict(trigger.tokenInterval, trigger.sentenceObj)
      argHead <- DependencyUtils.findHeadStrict(arg.tokenInterval, arg.sentenceObj)
    } yield deps.shortestPath(triggerHead, argHead, ignoreDirection = true)

    //println("Trigger: " + trigger.start + " -> " + trigger.end + " " + trigger.label)
    //println("Argument: " + arg.start + " -> " + arg.end + " " + arg.label)
    //println(s"Shortest path: ${shortestPath.get.mkString(", ")} in sentence ${trigger.sentenceObj.words.mkString(", ")}")

    shortestPath match {
      case None => Nil
      case Some(path) =>
        val shortestPathWithAdjMods = addAdjectivalModifiers(path, deps)
        val nnMods = nounModifiers(arg.tokenInterval.indices, deps)
        val ofMods = ofModifiers(arg.tokenInterval.indices, deps)
        // get all tokens considered negatives
        val negativesViaSyntax = for {
          tok <- (shortestPathWithAdjMods ++ nnMods ++ ofMods).distinct // a single token can't negate twice
          if !excluded.contains(tok)
          lemma = trigger.sentenceObj.lemmas.get(tok)
          if SEMANTIC_NEGATIVE_PATTERN.findFirstIn(lemma).isDefined
        } yield tok
        // return number of negatives
        (negativesViaSyntax ++ semanticNegativeIndicesInArgSpan(arg)).distinct
    }
  }


  /** Find index of semantic negatives in the arg's span. */
  def semanticNegativeIndicesInArgSpan(arg: Mention): Seq[Int] = arg match {
    // we can only safely count matches in the span for TBMs.  Other mentions require a purely syntactic approach
    case tbm: TextBoundMention =>
      val words = tbm.sentenceObj.words
      tbm.tokenInterval.filter(i => SEMANTIC_NEGATIVE_PATTERN.findFirstMatchIn(words(i)).nonEmpty)
    // FIXME: should this be a recursive call on the args?
    case _ => Nil
  }
  /**
    * Adds adjectival modifiers to all elements in the given path
    * This is necessary so we can properly inspect the semantic negatives,
    *   which are often not in the path, but modify tokens in it,
    *   "*decreased* PTPN13 expression increases phosphorylation of EphrinB1"
    */
  def addAdjectivalModifiers(tokens: Seq[Int], deps: DirectedGraph[String]): Seq[Int] = for {
    t <- tokens
    token <- t +: getModifiers(t, deps)
  } yield token

  def getModifiers(token: Int, deps: DirectedGraph[String]): Seq[Int] = for {
    (tok, dep) <- deps.getOutgoingEdges(token)
    if MODIFIER_LABELS.findFirstIn(dep).isDefined
  } yield tok

  def nounModifiers(tokens: Seq[Int], deps: DirectedGraph[String]): Seq[Int] = for {
    t <- tokens
    token <- t +: getNounModifiers(t, deps)
  } yield token

  def getNounModifiers(token: Int, deps: DirectedGraph[String]): Seq[Int] = for {
    (tok, dep) <- deps.getIncomingEdges(token) // NB: *Incoming* edges, for e.g. "Stat3 siRNA"
    if NOUN_LABELS.findFirstIn(dep).isDefined
  } yield tok

  def ofModifiers(tokens: Seq[Int], deps: DirectedGraph[String]): Seq[Int] = for {
    t <- tokens
    token <- t +: getOfModifiers(t, deps)
  } yield token

  def getOfModifiers(token: Int, deps: DirectedGraph[String]): Seq[Int] = for {
    (tok, dep) <- deps.getIncomingEdges(token) // NB: *Incoming* edges, for e.g. "knockdown of Stat3"
    if OF_LABELS.findFirstIn(dep).isDefined
  } yield tok

}
