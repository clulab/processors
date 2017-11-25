name := "processors-corenlp"

libraryDependencies ++= {
  Seq (
    // this sub-project depends on CoreNLP
    "edu.stanford.nlp"    %  "stanford-corenlp"  % "3.5.1",
    "edu.stanford.nlp"    %  "stanford-corenlp"  % "3.5.1" classifier "models"
  )
}
