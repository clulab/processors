package edu.arizona.sista.matcher

import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

object TestDependencyMatcher extends App {

  lazy val processor = new FastNLPProcessor()
  val examplesWithPatterns = Seq(
    ("My dog ate my homework.",
     """
     trigger: ate
     agent: nsubj
     patient: dobj
     """),

    ("My dog drove my mom to the store.",
     """
     trigger: drove
     agent: nsubj
     patient: dobj
     posstest: /^d/ > poss
     destination: prep
     """) // Unfortunately, FastNLP doesn't return collapsed deps
  )

  def processExamples(testPairs: Seq[(String, String)]) {
    for ((text, rule) <- testPairs) {
      val doc = processor.annotate(text)
      val s = doc.sentences(0)
      val dm = new DependencyMatcher(rule)

      // Our sentence...
      println(s"\n\n${{0 until s.words.length}.mkString("\t")}\n${s.words.mkString("\t")}")
      // Find any matches (currently all arguments are optional)
      val myMap = dm.findAllIn(s)

      // What do we get?
      println("\nOutgoing deps:")
      val deps = s.dependencies.get
      deps.outgoingEdges.zipWithIndex foreach { case (edges, i) => println(s"$i:\t${edges.mkString(" ")}") }
      println("\nAssigned labels:")
      myMap.flatten.foreach(pair => println(s"\t${pair._1} -> ${pair._2}"))
      println(s"\nRule:$rule\n")
    }
  }

  processExamples(examplesWithPatterns)
}
