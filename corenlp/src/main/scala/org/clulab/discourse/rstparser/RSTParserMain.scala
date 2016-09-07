package org.clulab.discourse.rstparser

import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.utils.StringUtils

/**
  * Main entry point for the RSTParser
  * User: mihais
  * Date: 9/6/16
  */
object RSTParserMain {
  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)
    var parser:RSTParser = null

    if(props.containsKey("train")) {
      parser = new RSTParser
      val (trees, cs) = CacheReader.mkTrees(
        props.getProperty("train"),
        props.containsKey("dep"))
      parser.train(trees, cs, props.containsKey("dep"))
      if(props.containsKey("model")) {
        parser.saveTo(props.getProperty("model"))
      }
    }

    if (props.containsKey("test")) {
      val modelPath =
        // if a model was provided, use it
        if (props.containsKey("model")) props.getProperty("model")
        // if the user wants dependencies, use that model
        else if (props.containsKey("dep")) RSTParser.DEFAULT_DEPENDENCYSYNTAX_MODEL_PATH
        // use constituent model by default
        else RSTParser.DEFAULT_CONSTITUENTSYNTAX_MODEL_PATH

      parser = RSTParser.loadFrom(modelPath)

      val (trees, _) = CacheReader.mkTrees(
        props.getProperty("test"),
        props.containsKey("dep"),
        makeStats = false)

      parser.test(trees)
    }

    if(props.containsKey("shell")) {
      var path = RSTParser.DEFAULT_CONSTITUENTSYNTAX_MODEL_PATH
      if(props.containsKey("dep")) path = RSTParser.DEFAULT_DEPENDENCYSYNTAX_MODEL_PATH

      if(parser == null && props.containsKey("model")) {
        parser = RSTParser.loadFrom(props.getProperty("model", path))
      } else {
        throw new RuntimeException("ERROR: property \"model\" or \"train\" must be specified!")
      }
      val proc = new CoreNLPProcessor()
      RSTParser.shell(parser, proc)
    }
  }
}
