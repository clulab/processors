name := "processors-corenlp"

libraryDependencies ++= {
  val corenlpV = "4.2.0"

  Seq (
    // this sub-project depends on CoreNLP
    "edu.stanford.nlp"    %  "stanford-corenlp"  % corenlpV,
    "edu.stanford.nlp"    %  "stanford-corenlp"  % corenlpV classifier "models",
  )
}
