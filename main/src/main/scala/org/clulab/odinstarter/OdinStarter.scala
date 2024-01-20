package org.clulab.odinstarter

import org.clulab.odin.ExtractorEngine
import org.clulab.odin.Mention
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.utils.FileUtils

import java.io.File

case class GraphNode(name: String, label: String, children: Seq[GraphNode] = Seq.empty)

object OdinStarter extends App {
  // When using an IDE rather than sbt, make sure the working directory for the run
  // configurati0on is the subproject directory so that this resourceDir is accessible.
  val resourceDir: File = new File("./src/main/resources")
  val customLexiconNer = { // i.e., Named Entity Recognizer
    val kbsAndCaseInsensitiveMatchings: Seq[(String, Boolean)] = Seq(
      // You can add additional kbs (knowledge bases) and caseInsensitiveMatchings here.
      ("org/clulab/odinstarter/FOOD.tsv", true) // Text to be parsed against the rules
      // ("org/clulab/odinstarter/RESTAURANTS.tsv", false)
    )
    val kbs = kbsAndCaseInsensitiveMatchings.map(_._1)
    val caseInsensitiveMatchings = kbsAndCaseInsensitiveMatchings.map(_._2)
    val isLocal = kbs.forall(new File(resourceDir, _).exists)
    val baseDirOpt = if (isLocal) Some(resourceDir) else None

    LexiconNER(kbs, caseInsensitiveMatchings, baseDirOpt)
  }

  val processor = new CluProcessor(optionalNER = Some(customLexiconNer))
  val extractorEngine = {
    val masterResource = "/org/clulab/odinstarter/main.yml"  // Loading the rules from main.yml
    // We usually want to reload rules during development,
    // so we try to load them from the filesystem first, then jar.
    // The resource must start with /, but the file probably shouldn't.
    val masterFile = new File(resourceDir, masterResource.drop(1))

    if (masterFile.exists) {
      // Read rules from file in filesystem.
      val rules = FileUtils.getTextFromFile(masterFile)
      ExtractorEngine(rules, ruleDir = Some(resourceDir))
    }
    else {
      // Read rules from resource in jar.
      val rules = FileUtils.getTextFromResource(masterResource)
      ExtractorEngine(rules, ruleDir = None)
    }
  }

  val document = processor.annotate("John eats cake.")
  val mentions = extractorEngine.extractFrom(document).sortBy(_.arguments.size)

  // Initializing the graph by creating a root node for the graph
  val rootNode = GraphNode("Root", "Root")

  for (mention <- mentions) {
    addMentionToGraphNode(rootNode, mention)
    printMention(mention)
  }

  visualizeGraph(rootNode, 2)

  def addMentionToGraphNode(parentNode: GraphNode, mention: Mention): Unit = {
    val node = GraphNode(mention.labels.headOption.getOrElse("None"), mention.text)

    val updatedParentNode = parentNode.copy(children = parentNode.children :+ node)

    // Recursively add children mentions
    for ((argName, argMentions) <- mention.arguments; childMention <- argMentions) {
      addMentionToGraphNode(updatedParentNode, childMention)
    }
  }

  def visualizeGraph(node: GraphNode, depth: Int): Unit = {
    val indent = "  " * depth
    println(s"$indent${node.name}: ${node.label}")

    // visualize children
    for (child <- node.children) {
      visualizeGraph(child, depth + 1)
    }
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
