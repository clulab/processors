package org.clulab.dynet

import java.io.{BufferedReader, File, FileReader, PrintWriter}

import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.serialization.DocumentSerializer
import org.clulab.struct.{Counter, DirectedGraph, GraphMap}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

import CoNLLSRLToMetal._


/**
  * Reads a CoNLL-2008 formatted file (containing semantic roles) and converts it to our own Metal format
  * User: mihais
  * Date: 5/5/15
  * Last Modified:
  *  08/05/2020: Added the latest Metal format
  *  Update for Scala 2.12: bug #10151 workaround.
  *
  */
class CoNLLSRLToMetal {
  class CoNLLToken(
    val word:String,
    val pos:String,
    val lemma:String,
    val dep:(Int, String), // head, label
    val pred:Int,
    val frameBits:Array[String]) {
    override def toString:String = word + "/" + pos + "/" + dep._1 + "/" + dep._2 + "/" + pred
  }

  var argConflictCount = 0
  var multiPredCount = 0
  var argCount = 0
  var predCount = 0

  def load(filePath:String):Document = {
    val serFile = new File(filePath + ".ser")
    if(serFile.exists()) {
      // if the serialized file exists, use it
      logger.debug(s"Found serialized file at ${serFile.getAbsolutePath}. Will use that.")
      val documentSerializer = new DocumentSerializer
      val b = new BufferedReader(new FileReader(serFile))
      val doc = documentSerializer.load(b)
      b.close()
      doc
    } else {
      // the serialized file does not exist!
      throw new RuntimeException(s"ERROR: Serialized file ${serFile.getAbsolutePath} does not exist! Please generate it using org.clulab.swirl2.ReaderMain.")
    }
  }

  def read(file:File,
           proc:Processor = null,
           verbose:Boolean = false):Document = {
    val source = Source.fromFile(file)
    val sentences = new ArrayBuffer[Array[CoNLLToken]]
    var sentence = new ArrayBuffer[CoNLLToken]

    argConflictCount = 0
    multiPredCount = 0
    argCount = 0
    predCount = 0
    var tokenCount = 0
    var sentCount = 0
    var hyphCount = 0

    //
    // read all sentences
    // also, collapse hyphenated phrases, which were brutally tokenized in CoNLL
    //
    for(l <- source.getLines()) {
      val line = l.trim
      if(line.length > 0) {
        val bits = l.split("\\t")
        // e println(s"LINE: $line")
        assert(bits.size >= 14)
        val token = mkToken(bits)
        sentence += token
        tokenCount += 1
        if(token.pos == "HYPH") hyphCount += 1
      } else {
        // end of sentence
        sentences += collapseHyphens(sentence.toArray, verbose)
        sentence = new ArrayBuffer[CoNLLToken]()
        sentCount += 1
      }
    }
    source.close()
    logger.debug(s"Read $tokenCount tokens, grouped in $sentCount sentences.")
    logger.debug(s"Found $hyphCount hyphens.")
    logger.debug(s"In hyphenated phrases, found $multiPredCount multi predicates and $argConflictCount argument conflicts.")

    //
    // construct the semantic roles from CoNLL tokens
    //
    val semDependencies = new ArrayBuffer[DirectedGraph[String]]()
    for(sent <- sentences) {
      semDependencies += mkSemanticDependencies(sent)
    }

    //
    // construct one Document for the entire corpus and annotate it
    //
    val document = mkDocument(sentences.toArray, proc)

    //
    // assign the semantic roles to sentences in the created Document
    //
    assert(document.sentences.length == semDependencies.size)
    for(i <- document.sentences.indices) {
      document.sentences(i).setDependencies(GraphMap.SEMANTIC_ROLES, semDependencies(i))
    }

    logger.debug(s"Found a total of $predCount predicates with $argCount arguments.")

    document
  }

  def mkDocument(sentences:Array[Array[CoNLLToken]], proc:Processor):Document = {
    //
    // create the document from tokens
    // then, regenerate the POS tags and syntactic dependencies
    //

    val tokens = sentences.map(_.map(_.word).toList).toList
    val doc = proc.mkDocumentFromTokens(tokens)

    /*
    if(USE_GOLD_SYNTAX) {
      // this only works with the original tokenization. TODO: fix this
      assert(USE_CONLL_TOKENIZATION)
      for(i <- sentences.indices) {
        val conllTokens = sentences(i)
        val sent = doc.sentences(i)
        sent.tags = Some(toTags(conllTokens))
        println(s"Using tags: ${sent.tags.get.toList}")
        sent.lemmas = Some(toLemmas(conllTokens))
      }
    } else {
      proc.tagPartsOfSpeech(doc)
      proc.lemmatize(doc)
    }
    */

    // Uncomment these lines if fancier features are needed!
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)

    /*
    if(USE_GOLD_SYNTAX) {
      // this only works with the original tokenization. TODO: fix this
      assert(USE_CONLL_TOKENIZATION)
      for(i <- sentences.indices) {
        val conllTokens = sentences(i)
        val sent = doc.sentences(i)
        val depGraph = toDirectedGraph(conllTokens)
        //println(depGraph)
        // we set the gold CoNLL syntax as Stanford basic dependencies (hack)
        sent.graphs += GraphMap.UNIVERSAL_BASIC -> depGraph
      }
    } else {
      proc.parse(doc)
    }
    */

    doc
  }

  def toTags(tokens:Array[CoNLLToken]):Array[String] = tokens.map(_.pos)

  def toLemmas(tokens:Array[CoNLLToken]):Array[String] = tokens.map(_.lemma)

  def toDirectedGraph(tokens:Array[CoNLLToken]):DirectedGraph[String] = {
    val edges = new mutable.ListBuffer[(Int, Int, String)] // head, modifier, label
    for(modifier <- tokens.indices) {
      val head = tokens(modifier).dep._1
      if(head >= 0)
        edges += Tuple3(head, modifier, tokens(modifier).dep._2)
      ()                                    // workaround for bug #10151
    }
    DirectedGraph[String](DirectedGraph.triplesToEdges[String](edges.toList))
  }

  def mkSemanticDependencies(sentence:Array[CoNLLToken]):DirectedGraph[String] = {
    val edges = new ListBuffer[(Int, Int, String)]
    val heads = new mutable.HashSet[Int]()
    val modifiers = new mutable.HashSet[Int]()

    var columnOffset = -1
    for(p <- sentence.indices) {
      if(sentence(p).pred > 0) { // found a head
        val head = p
        heads += head
        predCount += 1
        columnOffset += sentence(p).pred // in case of multiple predicates squished in one token, use the last
        for(i <- sentence.indices) {
          if(sentence(i).frameBits(columnOffset) != "_") {
            val modifier = i
            val label = simplifyLabel(sentence(i).frameBits(columnOffset))
            if(label.isDefined) {
              edges += Tuple3(head, modifier, label.get)
              modifiers += modifier
              argCount += 1
            }
          }
        }
      }
    }

    DirectedGraph[String](DirectedGraph.triplesToEdges[String](edges.toList))
  }

  val KEEP_LABELS = Set("A0", "A1", "R-A0", "R-A1", "AM-TMP", "AM-LOC", "AM-MOD", "AM-NEG")
  val AX_LABELS = Set("A2", "A3", "A4", "A5")

  def simplifyLabel(label:String): Option[String] = {
    if(! SIMPLIFY_ARG_LABELS) return Some(label)

    //
    // Keep: A0, A1, R-A0, R-A1, AM-TMP, AM-MNR, AM-LOC, AM-MOD, AM-ADV, AM-NEG
    // Change: A2-5 => Ax
    //
    if(KEEP_LABELS.contains(label)) Some(label)
    else if(AX_LABELS.contains(label)) Some("Ax")
    else None

  }

  def mkToken(bits:Array[String]):CoNLLToken = {
    val word = bits(1)
    val pos = bits(4)
    val lemma = bits(2)
    val head = bits(8).toInt - 1 // CoNLL offsets start at 1; ours start at 0
    val depLabel = bits(10)
    val isPred = bits(13) match {
      case "_" => 0
      case _ => 1
    }
    val frameBits =  bits.slice(14, bits.length)
    new CoNLLToken(word, pos, lemma, Tuple2(head, depLabel), isPred, frameBits)
  }

  /**
   * Merges tokens that were separated around dashes in CoNLL, to bring tokenization closer to the usual Treebank one
   * We need this because most parsers behave horribly if hyphenated words are tokenized around dashes
   */
  def collapseHyphens(origSentence:Array[CoNLLToken], verbose:Boolean):Array[CoNLLToken] = {
    if(USE_CONLL_TOKENIZATION) return origSentence

    val sent = new ArrayBuffer[CoNLLToken]()

    var start = 0
    while(start < origSentence.length) {
      val end = findEnd(origSentence, start)
      if(end > start + 1) {
        val token = mergeTokens(origSentence, start, end, verbose)
        sent += token
      } else {
        sent += origSentence(start)
      }
      start = end
    }

    sent.toArray
  }

  def findEnd(sent:Array[CoNLLToken], start:Int):Int = {
    var end = start + 1
    while(end < sent.length) {
      if(sent(end).pos != "HYPH") return end
      else end = end + 2
    }
    sent.length
  }

  def mergeTokens(sent:Array[CoNLLToken], start:Int, end:Int, verbose:Boolean):CoNLLToken = {
    val phrase = sent.slice(start, end)
    val word = phrase.map(_.word).mkString("")
    val pos = phrase.last.pos // this one doesn't really matter; we retag the entire data with our Processor anyway...
    val lemma = phrase.map(_.lemma).mkString("")
    val pred = mergePredicates(phrase, verbose)
    val frameBits = mergeFrames(phrase, verbose)

    if(verbose) {
      //logger.debug("Merging tokens: " + phrase.mkString(" ") + " as: " + word + "/" + isPred)
    }

    new CoNLLToken(word, pos, lemma, sent(start).dep, pred, frameBits) // TODO: fix this, generate correct collapsed CoNLL dependencies
  }

  def mergePredicates(phrase:Array[CoNLLToken], verbose:Boolean):Int = {
    val l = phrase.map(_.pred).sum

    if(l > 0) {
      if(l > 1) {
        if(verbose) logger.debug("Found MULTI PREDICATE in hyphenated phrase: " + phrase.mkString(" "))
        multiPredCount += 1
      }
      if(verbose) {
        // logger.info("Found hyphenated predicate: " + phrase.mkString(" "))
      }
    }

    l
  }

  def mergeFrames(phrase:Array[CoNLLToken], verbose:Boolean):Array[String] = {
    val frameBits = new Array[String](phrase(0).frameBits.length)
    for(i <- frameBits.indices) {
      frameBits(i) = mergeFrame(phrase, i, verbose)
    }
    frameBits
  }

  def mergeFrame(phrase:Array[CoNLLToken], position:Int, verbose:Boolean):String = {
    // pick the right-most argument assignment
    // for example, if the tokens have: "A1 _ A0" we would pick A0
    // of course, the above scenario is HIGHLY unlikely. normally, there will be a single argument, e.g.: "_ _ A0"

    var arg = "_"
    var count = 0
    for(i <- phrase.length - 1 to 0 by -1) {
      if(phrase(i).frameBits(position) != "_") {
        if(arg == "_") arg = phrase(i).frameBits(position)
        count += 1
      }
    }
    if(count > 1) {
      if(verbose) logger.debug("Found ARGUMENT CONFLICT " + phrase.map(_.frameBits(position)).mkString(" ") + " in hyphenated phrase: " + phrase.mkString(" "))
      argConflictCount += 1
    }

    arg
  }
}

object CoNLLSRLToMetal {
  val logger: Logger = LoggerFactory.getLogger(classOf[CoNLLSRLToMetal])

  val USE_CONLL_TOKENIZATION = false
  val SIMPLIFY_ARG_LABELS = true
  val REMOVE_SELF_LOOPS = true // do not allow self arguments for predicates

  //val USE_GOLD_SYNTAX = true

  def main(args: Array[String]): Unit = {
    assert(args.length == 2)
    Utils.initializeDyNet()

    val file = new File(args(0))
    val reader = new CoNLLSRLToMetal
    val proc = new CluProcessor() // FastNLPProcessor()
    val doc = reader.read(file, proc, verbose = true)

    labelStats(doc)
    moreStats(doc)

    // a simplified form of the CoNLL format
    //saveSimplified(doc, args(1))

    val predsFile = args(1) + ".preds"
    val argsFile = args(1) + ".args"

    // older Metal format, with one frame per sentence
    //saveMetal(doc, predsFile, argsFile)

    // new Metal format, with all frames saved in the same sentence
    saveMetalFull(doc, predsFile, argsFile)
  }

  def moreStats(document: Document): Unit = {
    var moreThanTwoPreds = 0
    var edgeCount = 0
    var multPreds = 0
    var multPredPerArgSents = 0
    val argPredHisto = new Counter[Int] // how many arguments with this many predicates
    for(s <- document.sentences) {
      if(s.semanticRoles.get.roots.size > 1) {
        moreThanTwoPreds += 1
        var isMultPredArg = false

        val headCounts = new Counter[Int]() // counts the number of preds for each arg
        for(edge <- s.semanticRoles.get.allEdges) {
          edgeCount += 1
          headCounts.incrementCount(edge._2) // _1 is the pred, _2 is the arg
        }
        for(arg <- headCounts.keySet) {
          if(headCounts.getCount(arg) > 1) {
            multPreds += 1
            isMultPredArg = true
          }
          argPredHisto.incrementCount(headCounts.getCount(arg).toInt)
        }

        if(isMultPredArg) multPredPerArgSents += 1
      }
    }

    println(s"Found $moreThanTwoPreds/${document.sentences.length} sentences with more than two predicates.")
    println(s"Found $multPredPerArgSents/${document.sentences.length} sentences where at least 1 arg has more than 1 predicate.")
    println(s"Out of $edgeCount (pred, arg) pairs, found $multPreds arguments with more than 1 predicate.")
    println(s"argPredHisto: ${argPredHisto.sorted(true).mkString(", ")}")
  }

  def saveMetal(doc: Document, predsFile: String, argsFile: String): Unit = {
    val predsPw = new PrintWriter(predsFile)
    val argsPw = new PrintWriter(argsFile)
    var selfLoopCount = 0

    for(sent <- doc.sentences) {
      val g = sent.graphs(GraphMap.SEMANTIC_ROLES)

      val heads = new Array[String](sent.words.length)
      for(i <- heads.indices) heads(i) = "O"
      var headPositions = new mutable.HashSet[Int]()
      for(e <- g.edges) {
        headPositions += e.source
        heads(e.source) = "B-P"
      }

      //
      // save predicate information
      //
      assert(heads.length == sent.words.length)
      for(i <- heads.indices) {
        predsPw.println(
          sent.words(i) + "\t" +
          heads(i) + "\t0\t" +
          sent.tags.get(i) + "\t" +
          sent.entities.get(i)
        )
      }

      //
      // save one frame for each predicate in the Metal format
      //
      val sortedHeadPositions = headPositions.toList.sorted
      val headMap = sortedHeadPositions.zipWithIndex.toMap

      val args = new Array[Array[String]](headMap.size)
      for(i <- args.indices) {
        args(i) = new Array[String](sent.size)
        for(j <- args(i).indices) args(i)(j) = "O"
      }

      for(e <- g.edges) {
        args(headMap(e.source))(e.destination) = e.relation

        if(REMOVE_SELF_LOOPS) {
          if(e.source == e.destination) {
            args(headMap(e.source))(e.destination) = "O"
            selfLoopCount += 1
          }
        }
      }

      // each frame saved separately
      assert(headMap.size == args.length)
      assert(sortedHeadPositions.size == args.length)
      for(fi <- args.indices) {
        val predPosition = sortedHeadPositions(fi)
        val frame = args(fi)

        assert(frame.length == sent.words.length)
        for(i <- frame.indices) {
          argsPw.println(
            sent.words(i) + "\t" +
            frame(i) + "\t" +
            predPosition + "\t" +
            sent.tags.get(i) + "\t" +
            sent.entities.get(i)
          )
        }
        argsPw.println()

      }

      predsPw.println()
    }

    predsPw.close()
    argsPw.close()

    if(REMOVE_SELF_LOOPS) {
      logger.info(s"Removed $selfLoopCount self-argument loops.")
    }
  }

  def saveMetalFull(doc: Document, predsFile: String, argsFile: String): Unit = {
    val predsPw = new PrintWriter(predsFile)
    val argsPw = new PrintWriter(argsFile)
    var selfLoopCount = 0

    for(sent <- doc.sentences) {
      val g = sent.graphs(GraphMap.SEMANTIC_ROLES)

      val heads = new Array[String](sent.words.length)
      for(i <- heads.indices) heads(i) = "O"
      var headPositions = new mutable.HashSet[Int]()
      for(e <- g.edges) {
        headPositions += e.source
        heads(e.source) = "B-P"
      }

      //
      // save predicate information
      //
      assert(heads.length == sent.words.length)
      for(i <- heads.indices) {
        predsPw.println(
          sent.words(i) + "\t" +
            sent.tags.get(i) + "\t" +
            sent.entities.get(i) + "\t" +
            heads(i)
        )
      }
      predsPw.println()

      //
      // save one frame for each predicate in the Metal format
      //
      if(headPositions.nonEmpty) {
        val sortedHeadPositions = headPositions.toList.sorted
        val headMap = sortedHeadPositions.zipWithIndex.toMap

        val args = new Array[Array[String]](headMap.size)
        for (i <- args.indices) {
          args(i) = new Array[String](sent.size)
          for (j <- args(i).indices) args(i)(j) = "O"
        }

        for (e <- g.edges) {
          args(headMap(e.source))(e.destination) = e.relation

          if (REMOVE_SELF_LOOPS) {
            if (e.source == e.destination) {
              args(headMap(e.source))(e.destination) = "O"
              selfLoopCount += 1
            }
          }
        }

        // save all frames together, as separate columns
        assert(headMap.size == args.length)
        assert(sortedHeadPositions.size == args.length)
        for (i <- sent.words.indices) {
          // word, POS tag, NE label
          argsPw.print(
            sent.words(i) + "\t" +
              sent.tags.get(i) + "\t" +
              sent.entities.get(i)
          )

          // (label, head position)+
          for (fi <- args.indices) {
            val predPosition = sortedHeadPositions(fi)
            val frame = args(fi)

            argsPw.print(
              "\t" + frame(i) +
                "\t" + predPosition
            )
          }
          argsPw.println()
        }

        argsPw.println()
      }
    }

    predsPw.close()
    argsPw.close()

    if(REMOVE_SELF_LOOPS) {
      logger.info(s"Removed $selfLoopCount self-argument loops.")
    }
  }

  def saveSimplified(doc: Document, outputFileName: String): Unit = {
    val pw = new PrintWriter(outputFileName)
    var selfLoopCount = 0

    for(sent <- doc.sentences) {
      val g = sent.graphs(GraphMap.SEMANTIC_ROLES)
      val heads = new Array[Boolean](sent.words.length)
      var headPositions = new mutable.HashSet[Int]()
      for(e <- g.edges) {
        headPositions += e.source
        heads(e.source) = true
      }

      val headMap = headPositions.toList.sorted.zipWithIndex.toMap

      val args = new Array[Array[String]](headMap.size)
      for(i <- args.indices) {
        args(i) = new Array[String](sent.size)
        for(j <- args(i).indices) args(i)(j) = "O"
      }

      for(e <- g.edges) {
        args(headMap(e.source))(e.destination) = e.relation

        if(REMOVE_SELF_LOOPS) {
          if(e.source == e.destination) {
            args(headMap(e.source))(e.destination) = "O"
            selfLoopCount += 1
          }
        }
      }

      for(i <- sent.words.indices) {
        pw.print(sent.words(i) + "\t" + (if(heads(i)) "B-P" else "O"))
        pw.print("\t" + sent.tags.get(i) + "\t" + sent.entities.get(i))
        for(j <- args.indices) {
          pw.print("\t" + args(j)(i))
        }
        pw.println()
      }
      pw.println()
    }
    pw.close()

    if(REMOVE_SELF_LOOPS) {
      logger.info(s"Removed $selfLoopCount self-argument loops.")
    }
  }

  def labelStats(doc: Document): Unit = {
    val labels = new Counter[String]()
    for(sent <- doc.sentences) {
      val g = sent.graphs(GraphMap.SEMANTIC_ROLES)
      for(e <- g.allEdges) {
        val l = e._3
        labels += l
      }
    }

    val pw = new PrintWriter("labels.tsv")
    for(l <- labels.sorted){
      pw.println(s"${l._1}\t${l._2}")
    }
    pw.close()

  }
}
