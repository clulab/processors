package org.clulab.processors


import org.clulab.processors.clu.BalaurProcessor
import org.clulab.struct.GraphMap
import org.clulab.utils.StringUtils
import org.slf4j.{Logger, LoggerFactory}

import java.io.{File, FileFilter, PrintWriter}
import scala.util.Using

import TextToCoNLLU._

/**
  * Processes raw text and saves the output in the CoNLL-U format
  * See http://universaldependencies.org/format.html for a description of this format
  *
  * @author Mihai
  */
class TextToCoNLLU(val proc:Processor, val isCoreNLP:Boolean) {
  def convert(inDir:File, outDir:File): Unit = {
    val inFiles = inDir.listFiles(new TextFileFilter)
    logger.info(s"Found ${inFiles.length} text file(s) to process.")
    for(f <- inFiles) {
      logger.debug(s"Parsing file $f...")
      try {
        val doc = parseFile(f)
        val ofn = s"$outDir/${f.getName.substring(0, f.getName.length - 4)}.conllu"
        Using.resource(new PrintWriter(ofn)) { pw =>
          toCoNLLU(doc, pw)
        }
      } catch {
        case e:Exception => {
          logger.error(s"Parsing of file $f failed with error:")
          e.printStackTrace()
        }

      }
    }
  }

  def toCoNLLU(doc:Document, pw:PrintWriter): Unit = {
    var sentenceCount = 0
    for(sent <- doc.sentences) {
      sentenceCount += 1
      pw.println(s"# Sentence #$sentenceCount:")
      pw.println(s"# ${sent.words.mkString(" ")}")

      val deps = sent.graphs(GraphMap.UNIVERSAL_BASIC).incomingEdges

      for(i <- 0 until sent.size) {
        val word = sent.words(i)
        val lemma = sent.lemmas.get(i)
        val upos = "_"
        val xpos = sent.tags.get(i)
        val feats = "_"
        val head =
          if(deps(i) != null && deps(i).nonEmpty) deps(i).head._1 + 1
          else "0"
        val deprel =
          if(deps(i) != null && deps(i).nonEmpty) deps(i).head._2
          else "root"
        pw.println(s"${i + 1}\t$word\t$lemma\t$upos\t$xpos\t$feats\t$head\t$deprel")
      }
      pw.println()
    }
  }

  def parseFile(f:File):Document = {
    val buffer = new StringBuilder
    Using.resource(scala.io.Source.fromFile(f)) { s =>
      for (line <- s.getLines()) {
        buffer.append(line)
        buffer.append("\n")
      }
    }

    val doc = proc.mkDocument(buffer.toString())
    annotate(doc)
    doc
  }

  def annotate(doc:Document): Unit = {
    if(isCoreNLP) {
      proc.tagPartsOfSpeech(doc)
      proc.lemmatize(doc)
    } else {
      proc.lemmatize(doc)
      proc.tagPartsOfSpeech(doc)
    }
    proc.parse(doc)
    doc.clear()
  }
}

class TextFileFilter extends FileFilter {
  override def accept(pathname: File): Boolean = pathname.getName.endsWith(".txt")
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

    val props = StringUtils.argsToMap(args)

    val proc = Processor()
    val converter = new TextToCoNLLU(proc, false)

    val inDirName = props.getOrElse("indir", {
      usage()
      throw new RuntimeException("""Missing argument "indir"!""")
    })
    val inDir = new File(inDirName)
    if(! inDir.isDirectory) {
      usage()
      throw new RuntimeException("""Argument "indir" must point to a valid directory!""")
    }

    val outDirName = props.getOrElse("outdir", {
      usage()
      throw new RuntimeException("""Missing argument "outdir"!""")
    })
    val outDir = new File(outDirName)
    if(! outDir.isDirectory) {
      usage()
      throw new RuntimeException("""Argument "outdir" must point to a valid directory!""")
    }

    converter.convert(inDir, outDir)
  }
}
