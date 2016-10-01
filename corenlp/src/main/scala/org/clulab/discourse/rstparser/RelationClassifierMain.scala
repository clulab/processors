package org.clulab.discourse.rstparser

import java.io._

import org.clulab.utils.StringUtils

/**
  *
  * User: mihais
  * Date: 9/6/16
  */
object RelationClassifierMain {
  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)

    var cls:RelationClassifier = null

    var prefixes = RelationClassifier.CONSTITUENTSYNTAX_PREFIXES
    if(props.containsKey("dep")) prefixes = RelationClassifier.DEPENDENCYSYNTAX_PREFIXES

    if(props.containsKey("train")) {
      cls = new RelationClassifier (prefixes, withNuclearity = true)
      val (trees, corpusStats) =
        CacheReader.mkTrees(props.getProperty("train"),
                            props.containsKey("dep"))
      cls.train(trees, corpusStats)
      if(props.containsKey("model")) {
        val os = new PrintWriter(new BufferedWriter(new FileWriter(props.getProperty("model"))))
        cls.saveTo(os, saveCorpusStats = true)
        os.close()
      }
    }
    if(props.containsKey("test")) {
      val (trees, _) =
        CacheReader.mkTrees(props.getProperty("test"),
          props.containsKey("dep"),
          makeStats = false)
      if(props.containsKey("model")) {
        val is = new BufferedReader(new FileReader(props.getProperty("model")))
        cls = RelationClassifier.loadFrom(is, corpusStats = null)
        is.close()
      }
      cls.test(trees)
    }
    if(props.containsKey("fsel")) {
      cls = new RelationClassifier (null, withNuclearity = true)
      val (trees, corpusStats) =
        CacheReader.mkTrees(props.getProperty("fsel"),
                            props.containsKey("dep"))
      cls.featureSelectionIncremental(trees, corpusStats)
    }
  }
}
