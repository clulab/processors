package edu.arizona.sista.processors.visualizer

import scala.collection.JavaConverters._

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

import edu.arizona.sista.processors._
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
 * External API for running different discourse parsers for visualization.
 * Written By: Tom Hicks. 1/15/2015.
 * Last Modified: Simplify timings to list of lists, return JSON.
 */
class DiscourseParserRunner (useProcessor:String = "core") {
  val processor:Processor =
    if (useProcessor == "fast")             // fast but slightly worse discourse parser
      new FastNLPProcessor(withDiscourse = true)
    else                                    // default: slow but better discourse parser
      new CoreNLPProcessor(withDiscourse = true)


  def parseText (text: String): DiscourseParserResults = {
    // create and annotate a document using the selected processor
    val start = System.currentTimeMillis()
    // val doc = processor.annotate(text)
    // var timings:List[List[Any]] = List()
    var (doc, timings) = myAnnotate(processor.mkDocument(text)) // call for custom processing
    val stop = System.currentTimeMillis()
    val elapsed = stop - start
    timings = List("ANNOT", elapsed, start, stop) +: timings
//    println(this.toString()+": [ANNOT]: %5d, %d, %d".format(elapsed, start, stop))

    doc.discourseTree.foreach(dt => {
      println(this.toString()+": Discourse tree from processor.annotate (with coRef)")
      println(dt.toString())
    })

    // return information from discourse trees as an array of JSON strings:
    new DiscourseParserResults(text, toJson(timings), discTrees(doc), synTrees(doc))
  }


  // this alternate annotate method can be called from above to customize processing
  def myAnnotate (doc:Document): (Document, List[List[Any]]) = {
    var t:List[List[Any]] = List(
      "POS"   +: timeIt { processor.tagPartsOfSpeech(doc) },
      "Lemma" +: timeIt { processor.lemmatize(doc) },
      "NER"   +: timeIt { processor.recognizeNamedEntities(doc) },
      "Parse" +: timeIt { processor.parse(doc) },
      "Chunk" +: timeIt { processor.chunking(doc) },
      "Roles" +: timeIt { processor.labelSemanticRoles(doc) },
      "CoRef" +: timeIt { processor.resolveCoreference(doc) },
      "DiscP" +: timeIt { processor.discourse(doc) }
    )
    doc.clear()
    return (doc, t)
  }

  def discTrees (doc: Document): Array[String] = {
    val allTrees = doc.discourseTree map { dTree =>
      dTree.visualizerJSON()
    }
    allTrees.toArray
  }

  def synTrees (doc: Document): Array[String] = {
    val allTrees = doc.sentences map { s =>
      s.syntacticTree.getOrElse("()").toString()
    }
    allTrees.toArray
  }

  // return a map of start time, stop time, and elapsed time for the given block
  def timeIt (it: => Unit): List[Long] = {
    val start = System.currentTimeMillis()
    it
    val stop = System.currentTimeMillis()
    return List((stop-start), start, stop)
  }

  // return a JSON string from the given list of lists
  def toJson (timings:List[List[Any]], pprint:Boolean=false): String = {
    val jTimings:List[JValue] = timings.map(it => tListToJson(it))
    if (pprint)
      pretty(render(jTimings))
    else
      compact(render(jTimings))
  }

  def tListToJson (lst:List[Any]): JArray = {
    JArray(List(JString(lst(0).toString),
                JInt(BigInt(lst(1).asInstanceOf[Long])),
                JInt(BigInt(lst(2).asInstanceOf[Long])),
                JInt(BigInt(lst(3).asInstanceOf[Long]))))
  }

  // override and customize string representation of this class
  override def toString:String = {
    return "<%s:%s>".format(super.getClass().getSimpleName(), processor.getClass.getSimpleName())
  }
}


/** Class to hold parser results. */
class DiscourseParserResults(val text:String,
                             val timings:String,
                             val dTrees:Array[String],
                             val synTrees:Array[String])
