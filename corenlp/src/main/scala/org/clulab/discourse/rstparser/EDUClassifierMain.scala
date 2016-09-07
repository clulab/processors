package org.clulab.discourse.rstparser

import java.io._

import org.clulab.utils.StringUtils

/**
  *
  * User: mihais
  * Date: 9/6/16
  */
object EDUClassifierMain {
  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)
    var cls:EDUClassifier = null

    if(props.containsKey("train")) {
      cls = new EDUClassifier
      val (trees, corpusStats) =
        CacheReader.mkTrees(props.getProperty("train"),
                            props.containsKey("dep"))
      cls.train(trees, corpusStats)
      if(props.containsKey("model")) {
        val os = new PrintWriter(new BufferedWriter(new FileWriter(props.getProperty("model"))))
        cls.saveTo(os)
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
        cls = EDUClassifier.loadFrom(is)
        is.close()
      }
      cls.test(trees)
    }
    if(props.containsKey("fsel")) {
      cls = new EDUClassifier
      val (trees, corpusStats) =
        CacheReader.mkTrees(props.getProperty("fsel"),
                            props.containsKey("dep"))
      cls.featureSelectionIncremental(trees, corpusStats)
    }
  }
}
