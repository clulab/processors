package edu.arizona.sista.swirl2

import java.io.{FileReader, BufferedReader, PrintWriter, File}

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.{DependencyMap, DocumentSerializer, Document, Processor}
import edu.arizona.sista.struct.DirectedGraph
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.io.Source

import Reader._

/**
 * Reads a CoNLL formatted file and converts it to our own representation
 * User: mihais
 * Date: 5/5/15
 */
class Reader {
  class CoNLLToken(val word:String, val pos:String, val pred:Int, val frameBits:Array[String]) {
    override def toString:String = word + "/" + pos + "/" + pred  }

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
      // the serialized file does not exist. Parse online
      val proc = new FastNLPProcessor(useMalt = false)
      read(new File(filePath), proc)
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
    for(i <- 0 until document.sentences.length) {
      document.sentences(i).setDependencies(DependencyMap.SEMANTIC_ROLES, semDependencies(i))
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
    val doc = proc.mkDocumentFromTokens(tokens, keepText = false)
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    proc.parse(doc)

    doc
  }

  def mkSemanticDependencies(sentence:Array[CoNLLToken]):DirectedGraph[String] = {
    val edges = new ListBuffer[(Int, Int, String)]
    val heads = new mutable.HashSet[Int]()
    val modifiers = new mutable.HashSet[Int]()

    var columnOffset = -1
    for(p <- 0 until sentence.length) {
      if(sentence(p).pred > 0) { // found a head
        val head = p
        heads += head
        predCount += 1
        columnOffset += sentence(p).pred // in case of multiple predicates squished in one token, use the last
        for(i <- 0 until sentence.length) {
          if(sentence(i).frameBits(columnOffset) != "_") {
            val modifier = i
            val label = sentence(i).frameBits(columnOffset)
            edges += new Tuple3(head, modifier, label)
            modifiers += modifier
            argCount += 1
          }
        }
      }
    }

    val roots = new mutable.HashSet[Int]()
    for(h <- heads) {
      if(! modifiers.contains(h)) {
        roots += h
      }
    }

    new DirectedGraph[String](edges.toList, roots.toSet)
  }

  def mkToken(bits:Array[String]):CoNLLToken = {
    val word = bits(1)
    val pos = bits(4)
    val isPred = bits(13) match {
      case "_" => 0
      case _ => 1
    }
    val frameBits =  bits.slice(14, bits.length)
    new CoNLLToken(word, pos, isPred, frameBits)
  }

  /**
   * Merges tokens that were separated around dashes in CoNLL, to bring tokenization closer to the usual Treebank one
   * We need this because most parsers behave horribly if hyphenated words are tokenized around dashes
   */
  def collapseHyphens(origSentence:Array[CoNLLToken], verbose:Boolean):Array[CoNLLToken] = {
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
    val pred = mergePredicates(phrase, verbose)
    val frameBits = mergeFrames(phrase, verbose)

    if(verbose) {
      //logger.debug("Merging tokens: " + phrase.mkString(" ") + " as: " + word + "/" + isPred)
    }

    new CoNLLToken(word, pos, pred, frameBits)
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
    for(i <- 0 until frameBits.length) {
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

object Reader {
  val logger = LoggerFactory.getLogger(classOf[Reader])

  def main(args:Array[String]) {
    val reader = new Reader
    val proc = new FastNLPProcessor(useMalt = false)
    val file = new File(args(0))
    val outputFile = new File(args(0) + ".ser")

    val doc = reader.read(file, proc, verbose = false)
    val serializer = new DocumentSerializer
    val os = new PrintWriter(outputFile)
    serializer.save(doc, os)
    os.close()
  }
}
