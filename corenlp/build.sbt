name := "processors-corenlp"

libraryDependencies ++= {
  val corenlpV = "3.8.0"

  Seq (
    // this sub-project depends on CoreNLP
    "edu.stanford.nlp"    %  "stanford-corenlp"  % corenlpV,
    "edu.stanford.nlp"    %  "stanford-corenlp"  % corenlpV classifier "models",

    "org.clulab" % "bioresources" % "1.1.24" // for bio NER
  )
}
