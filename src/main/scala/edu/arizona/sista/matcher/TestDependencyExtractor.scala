package edu.arizona.sista.matcher

import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.matcher.dependencies.DependencyExtractor

object TestDependencyExtractor extends App {

  lazy val processor = new CoreNLPProcessor()
  val examplesWithPatterns = Seq(
    ("My dog ate my homework.",
     """
     trigger: [lemma=eat]
     agent: nsubj
     patient: dobj
     """),

    ("My dog drove my mom to the store and the park.",
     """
     trigger: [lemma=drive]
     agent: nsubj
     patient: dobj
     posstest: /^d/ > "poss"
     posstest2: /^d/ /po/
     destination: prep_to
     destination2: prep_to [word=park]
     destination3: 'prep_to' [word=park | word=/^s/]
     destination4: "prep_to" [!word="park"]
     destination5: prep_to [word=park] < /^conj/
     destination6: prep_to [word=park | word=/^p/]
     or_test: (nsubj | dobj) /^p/
     or_test2: dobj | dobj
     """)
  )

  def processExamples(testPairs: Seq[(String, String)]) {
    for ((text, rule) <- testPairs) {
      val doc = processor.annotate(text)
      val s = doc.sentences(0)
      val dm = new DependencyExtractor(rule)

      // Our sentence...
      println(s"\n\n${{0 until s.words.length}.mkString("\t")}\n${s.words.mkString("\t")}")
      // Find any matches (currently all arguments are optional)
      val myMap = dm.findAllIn(s)

      // What do we get?
      println("\nOutgoing deps:")
      val deps = s.dependencies.get
      deps.outgoingEdges.zipWithIndex foreach {
        case (edges, i) => println(s"$i:\t${edges.mkString(" ")}")
      }
      println("\nAssigned labels:")
      myMap.flatten.foreach(pair => println(s"\t${pair._1} -> ${pair._2}"))
      println(s"\nRule:$rule\n")
    }
  }

  processExamples(examplesWithPatterns)
}
