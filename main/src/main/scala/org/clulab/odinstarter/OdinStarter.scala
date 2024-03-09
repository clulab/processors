package org.clulab.odinstarter

import org.clulab.odin.{Actions, ExtractorEngine, Mention, identityAction}
import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, GraphPattern, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, RuleReader, SaveEnd, SaveStart, Split, TokenExtractor, TokenPattern}
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.utils.FileUtils

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8

object OdinStarter extends App {
  // When using an IDE rather than sbt, make sure the working directory for the run
  // configuration is the subproject directory so that this resourceDir is accessible.
  val resourceDir: File = new File("./src/main/resources")
  val customLexiconNer = { // i.e., Named Entity Recognizer
    val kbsAndCaseInsensitiveMatchings: Seq[(String, Boolean)] = Seq(
      // You can add additional kbs (knowledge bases) and caseInsensitiveMatchings here.
      ("org/clulab/odinstarter/FOOD.tsv", true) // ,
      // ("org/clulab/odinstarter/RESTAURANTS.tsv", false)
    )
    val kbs = kbsAndCaseInsensitiveMatchings.map(_._1)
    val caseInsensitiveMatchings = kbsAndCaseInsensitiveMatchings.map(_._2)
    val isLocal = kbs.forall(new File(resourceDir, _).exists)
    val baseDirOpt = if (isLocal) Some(resourceDir) else None

    LexiconNER(kbs, caseInsensitiveMatchings, baseDirOpt)
  }
  val processor = new CluProcessor(optionalNER = Some(customLexiconNer))
  val (rules, ruleDirOpt) = {
    val masterResource = "/org/clulab/odinstarter/main.yml"
    // We usually want to reload rules during development,
    // so we try to load them from the filesystem first, then jar.
    // The resource must start with /, but the file probably shouldn't.
    val masterFile = new File(resourceDir, masterResource.drop(1))

    if (masterFile.exists) {
      // Read rules from file in filesystem.
      val rules = FileUtils.getTextFromFile(masterFile)
      val ruleDirOpt = Some(resourceDir)

      (rules, ruleDirOpt)
    }
    else {
      // Read rules from resource in jar.
      val rules = FileUtils.getTextFromResource(masterResource)
      val ruleDirOpt = None

      (rules, ruleDirOpt)
    }
  }
  val reader = new RuleReader(new Actions, UTF_8, ruleDirOpt)
  val extractors = reader.read(rules)
  val extractorEngine = new ExtractorEngine(extractors, identityAction)
  val document = processor.annotate("John eats cake.")
  val mentions = extractorEngine.extractFrom(document).sortBy(_.arguments.size)

  for (mention <- mentions)
    printMention(mention)

  for (extractor <- extractors)
    visualize(extractor)

  def visualize(extractor: Extractor): Unit = {
    extractor match {
      case tokenExtractor: TokenExtractor => visualizeExtractor(tokenExtractor.pattern.start, tokenExtractor.name)
      case graphExtractor: GraphExtractor => println("There was a graph extractor.")
      case crossSentenceExtractor: CrossSentenceExtractor =>
        visualizeExtractor(crossSentenceExtractor.anchorPattern.pattern.start, s"${crossSentenceExtractor.name} (Anchor)")
        visualizeExtractor(crossSentenceExtractor.neighborPattern.pattern.start, s"${crossSentenceExtractor.name} (Neighbor)")
      case _ => println("Unknown extractor type")
    }
  }

  private def visualizeExtractor(inst: Inst, name: String): Unit = {

    def loopsOrDeadEnds(nextInst: Inst): Boolean = {
      nextInst == null || (nextInst.getPosId <= inst.getPosId && nextInst.getPosId != 0)
    }

    val visualization = inst.visualize()

    println(s"There was an extractor: $name - Inst: $visualization")

    inst match {
      case split: Split =>
        if (!loopsOrDeadEnds(split.lhs))
          visualizeExtractor(split.lhs, s"$name (LHS)")
        if (!loopsOrDeadEnds(split.rhs))
          visualizeExtractor(split.rhs, s"$name (RHS)")

      case saveStart: SaveStart =>
        println(saveStart.visualize())

      case saveEnd: SaveEnd =>
        println(saveEnd.visualize())

      case matchToken: MatchToken =>
        println(matchToken.visualize())

      case matchMention: MatchMention =>
        println(matchMention.visualize())

      case sentenceStart: MatchSentenceStart =>
        println(sentenceStart.visualize())

      case sentenceEnd: MatchSentenceEnd =>
        println(sentenceEnd.visualize())

      case lookAhead: MatchLookAhead =>
        if (!loopsOrDeadEnds(lookAhead.start))
          visualizeExtractor(lookAhead.start, s"$name (Start)")
      case lookBehind: MatchLookBehind =>
        if (!loopsOrDeadEnds(lookBehind.start))
          visualizeExtractor(lookBehind.start, s"$name (Start)")

      case _ =>
    }
    if (!loopsOrDeadEnds(inst.getNext))
      visualizeExtractor(inst.getNext, s"$name (Next)")
  }


  def printMention(mention: Mention, nameOpt: Option[String] = None, depth: Int = 0): Unit = {
    val indent = "    " * depth
    val name = nameOpt.getOrElse("<none>")
    val labels = mention.labels
    val words = mention.sentenceObj.words
    val tokens = mention.tokenInterval.map(mention.sentenceObj.words)

    println(indent + "     Name: " + name)
    println(indent + "   Labels: " + labels.mkString(" "))
    println(indent + " Sentence: " +  words.mkString(" "))
    println(indent + "   Tokens: " + tokens.mkString(" "))
    if (mention.arguments.nonEmpty) {
      println(indent + "Arguments:")
      for ((name, mentions) <- mention.arguments; mention <- mentions)
        printMention(mention, Some(name), depth + 1)
    }
    println()
  }
}