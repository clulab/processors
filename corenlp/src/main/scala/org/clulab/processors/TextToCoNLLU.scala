package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.utils.StringUtils
import org.slf4j.{Logger, LoggerFactory}

/**
  * Processes raw text and saves the output in the CoNLL-U format
  * See http://universaldependencies.org/format.html for a description of this format
  *
  * @author Mihai
  */
class TextToCoNLLU {

}

object TextToCoNLLU {
  val logger: Logger = LoggerFactory.getLogger(classOf[TextToCoNLLU])

  def usage(): Unit = {
    println("Usage: org.clulab.processors.TextToCoNLLU -indir <input directory with text file> -outdir <output directory> -proc [clu]|corenlp")
  }

  def main(args:Array[String]): Unit = {
    if(args.length == 0) {
      usage()
      System.exit(0)
    }

    val props = StringUtils.argsToProperties(args)

    val proc =
      if(props.containsKey("proc") && props.getProperty("proc") == "corenlp") new FastNLPProcessor()
      else new CluProcessor()

    val inDir = props.getProperty("indir", "")
    if(inDir == "") {
      usage()
      throw new RuntimeException("""Missing argument "indir"!""")
    }


  }
}
