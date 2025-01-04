package org.clulab.processors.apps

import org.clulab.processors.Document
import org.clulab.processors.clu.BalaurProcessor
import org.clulab.serialization.CoNLLUSerializer
import org.clulab.utils.{FileUtils, StringUtils}

import java.io.PrintWriter

object CommandLineInterface extends App {
  // specifies the input file
  val INPUT = "input"
  // specifies the output file; if not provided uses stdout
  val OUTPUT = "output"

  // input formats:
  val RAW = "raw"
  val SENTS = "sents"
  val TOKENS = "tokens"

  val props = StringUtils.argsToProperties(args)
  println(props)

  if (!props.containsKey(INPUT)) {
    // run in interactive mode because no input file was provided
    new ProcessorsShell().shell()
  } else {
    // process a file in batch mode
    val proc = new BalaurProcessor()

    val doc: Document =
      if (props.containsKey(RAW)) {
        val rawText = FileUtils.getTextFromFile(props.getProperty(INPUT))
        proc.annotate(rawText)
      } else {
        throw new RuntimeException(s"ERROR: unknown input file format. Please use one of: $RAW|$SENTS|$TOKENS.")
      }

    val pw = mkOutput()
    CoNLLUSerializer.saveCoNLLUExtended(pw, doc)
    pw.close()
  }

  def mkOutput(): PrintWriter = {
    if(props.containsKey(OUTPUT)) FileUtils.printWriterFromFile(props.getProperty(OUTPUT))
    else new PrintWriter(System.out)
  }
}
