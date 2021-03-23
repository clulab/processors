name := "processors-corenlp"

libraryDependencies ++= {
  val corenlpV = "3.9.2"

  Seq (
    // this sub-project depends on CoreNLP
    "edu.stanford.nlp"    %  "stanford-corenlp"  % corenlpV,
    "edu.stanford.nlp"    %  "stanford-corenlp"  % corenlpV classifier "models"
  )
}
