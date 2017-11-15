package org.clulab.ie

import java.util

import org.clulab.odin.impl.Taxonomy
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor


/**
  * Originally from influencer project
  * Contains word lists and patterns for filtering entities (transparent terms, stopwords, etc.)
  */
object VocabularyConstraints {

  // Taxonomy object
  val taxonomy = readTaxonomy("org/clulab/grammar/taxonomy.yml")

  private def readTaxonomy(path: String): Taxonomy = {
    val url = getClass.getClassLoader.getResource(path)
    val source = if (url == null) scala.io.Source.fromFile(path) else scala.io.Source.fromURL(url)
    val input = source.mkString
    source.close()
    val yaml = new Yaml(new Constructor(classOf[util.Collection[Any]]))
    val data = yaml.load(input).asInstanceOf[util.Collection[Any]]
    Taxonomy(data)
  }

  val NODES = taxonomy.hyponymsFor("Node").toSet

  // add RB to grab "not"?
  val VALIDTAG = """^(NN|JJ|VB).*"""
  val VALID_FINAL_TAG = """^(NN|VB).*"""

  // a subset of punctuation that we want to avoid
  // should we use \p{Punct} instead?
  val PUNCT =
  """.*[%\.\]\[\(\)].*"""


  // FIXME: sniff these (and similar words) out automagically
  // these should use the lemma form
  // and be listed in alphabetical order (for ease of editing)
  val Transparent: Set[String] = Set(
    "ability", "achievement", "activity", "addition", "adjustment", "administration", "adult", "agent", "aim", "algorithm", "alteration", "amount", "approach", "attempt", "auc", "author", "authors",
    "belief", "barrier", "body",
    "capacity", "case", "category", "child", "childhood", "change", "cohort", "combination", "community", "concentration", "concept", "concern", "conclusion", "condition", "consequence", "consumption", "contrast", "country", "cow", "curve",
    "datum", "development", "diagnosis", "difference",
    "effect", "effort", "estimate", "evidence", "exacerbation", "exposure", "expression", "extent",
    "factor", "finding", "form", "formation", "frequency",
    "gene", "goal", "group", "growth", "guideline",
    "household", "hour", "human", "hypothesis",
    "idea", "importance", "improvement", "incidence", "indication", "individual", "insight", "intake", "intervention",
    "kind", "level", "lifestyle",
    "man", "manuscript", "many", "measure", "measurement", "mechanism", "mg", "more", "mouse",
    "need", "notion", "number",
    "observation", "occurrence", "odds", "opinion", "origin",
    "patient", "part", "percent", "performance", "persistence", "phenotype", "pool", "population", "possibility", "presence", "prevalence", "priority", "program", "progression", "project", "proliferation", "proposal",
    "range", "rat", "rate", "reason", "recognition", "referral", "risk", "role",
    "safety", "sample", "series", "size", "state", "status", "step", "strategy", "study", "substance", "suggestion", "susceptibility", "symptom",
    "technique", "time", "treatment", "turn", "type",
    "understanding", "use",
    "variable", "various", "view",
    "way", "woman", "work", "year"
  )

  val StopWords: Set[String] = Set(
    "age", "background", "common", "fact", "first", "hour",
    "introduction", "invitation", "invitee",
    "other", "paper", "procedure",
    "question", "revision", "test", "trial", "wide", "word",
    "xref_bibr", "xref_fig", "xref_table", "xref_supplementary"
  )

  // these should use the lemma form
  // whenever one of these phrases is encountered, the mention should be discarded
  val StopPhrases: Set[String] = Set(
    "effective way", "manuscript", "other word", "grant", "regression model", "="
  )
}

