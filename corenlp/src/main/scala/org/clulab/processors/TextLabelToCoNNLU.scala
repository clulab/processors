package org.clulab.processors

import java.io.{File, FileFilter, PrintWriter}
import org.clulab.processors.clu.{CluProcessor, GivenConstEmbeddingsAttachment}
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.utils.{FileUtils, Sourcer, StringUtils}
import org.slf4j.{Logger, LoggerFactory}
import TextLabelToCoNLLU._
import org.clulab.dynet.Utils
import org.clulab.struct.GraphMap
import org.clulab.utils.Closer.AutoCloser

/**
  * Processes raw text and saves the output in the CoNLL-U format
  * See http://universaldependencies.org/format.html for a description of this format
  *
  * @author Mihai
  */
class TextLabelToCoNLLU(val proc:Processor, val isCoreNLP:Boolean) {
  def convert(inDir:File, outDir:File): Unit = {
    val inFiles = inDir.listFiles(new TextLabelFileFilter)
    logger.info(s"Found ${inFiles.length} text file(s) to process.")
    for(f <- inFiles) {
      logger.debug(s"Parsing file $f...")
      try {
        val doc = parseFile(f)
        val ofn = s"$outDir/${f.getName.substring(0, f.getName.length - 4)}.conllu"
        val pw = new PrintWriter(ofn)
        toCoNLLU(doc, pw)
        pw.close()
      } catch {
        case e:Exception => {
          logger.error(s"Parsing of file $f failed with error:")
          e.printStackTrace()
        }

      }
    }
  }

  def toCoNLLU(doc:Document, pw:PrintWriter) {
    var sentenceCount = 0
    for(sent <- doc.sentences) {
      println(sent)
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
        
        // Generate labels (UI, L, AU)
        val textLabel =
          if(word.toUpperCase().equals(word) && (word.filter(_.isLetter)).length != 0) "UA"
          else if (Character.isUpperCase(word.charAt(0))) "UI"
          else "L"
        
        // Lower case the text
        val lowerCasedWord = word.toLowerCase
        pw.println(s"${i + 1}\t$lowerCasedWord\t$textLabel")

      }
      pw.println()
    }
  }

  def parseFile(f:File):Document = {
    def option1(): Document = {
      val tokens = Sourcer.sourceFromFile(f).autoClose { source =>
        for (line <- source.getLines())
          yield line.split(' ').toSeq
      }.toSeq
      println(tokens)
      proc.mkDocumentFromTokens(tokens)
    }

    def option2(): Document = {
      val text = FileUtils.getTextFromFile(f)
      proc.mkDocument(text)
    }

    val doc = option2()
    annotate(doc)
    doc
  }

  def annotate(doc:Document): Unit = {
    if(isCoreNLP) {
      proc.tagPartsOfSpeech(doc)
      proc.lemmatize(doc)
      proc.parse(doc)
    } else {
      GivenConstEmbeddingsAttachment(doc).perform {
        proc.lemmatize(doc)
        proc.tagPartsOfSpeech(doc)
        proc.recognizeNamedEntities(doc)
        proc.parse(doc)
      }
    }
    doc.clear()
  }
}

class TextLabelFileFilter extends FileFilter {
  override def accept(pathname: File): Boolean = {
    if(pathname.getName.endsWith(".txt")) {
      return true
    }
    false
  }
}

object TextLabelToCoNLLU {
  val logger: Logger = LoggerFactory.getLogger(classOf[TextLabelToCoNLLU])

  def usage(): Unit = {
    println("Usage: org.clulab.processors.TextLabelToCoNLLU -indir <input directory with text file> -outdir <output directory> -proc [clu]|corenlp")
  }

  def main(args:Array[String]): Unit = {
    if(args.length == 0) {
      usage()
      System.exit(0)
    }

    val props = StringUtils.argsToMap(args)

    val proc =
      if (props.get("proc").exists(_ == "corenlp")) new FastNLPProcessor()
      else {
        Utils.initializeDyNet()
        new CluProcessor()
      }
    val isCoreNLP = props.get("proc").exists(_ == "corenlp")
    val converter = new TextLabelToCoNLLU(proc, isCoreNLP)

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

