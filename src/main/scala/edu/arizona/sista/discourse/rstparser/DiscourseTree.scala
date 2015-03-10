package edu.arizona.sista.discourse.rstparser

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

import edu.arizona.sista.processors.Document

/**
  * Stores the RST discourse tree for one document
  * User: mihais
  * Date: 4/4/14
  * Last Modified: Document JSON output methods added for discourse parser visualizer.
 */
@SerialVersionUID(1L)
class DiscourseTree (
  /** Label of this tree, if non-terminal */
  var relationLabel:String,
  /** Direction of the relation, if non-terminal */
  var relationDirection:RelationDirection.Value,

  /** Children of this non-terminal node */
  var children:Array[DiscourseTree],

  /** Nucleus or Satellite; used only during reading */
  val kind:TreeKind.Value,

  /** Raw text attached to this node */
  val rawText:String,

  /** Character offsets for the rawText; used only during reading */
  var charOffsets:(Int, Int),

  /** Position of the first token in the annotation for this discourse tree */
  var firstToken: TokenOffset = null,

  /** Position of the last token in the annotation for this discourse tree; this is inclusive! */
  var lastToken: TokenOffset = null,

  /** Index of the first EDU in this tree in the array of EDUs for the first sentence */
  var firstEDU: Int = -1,

  /** Index of the last EDU in this tree in the array of EDUs for the last sentence (inclusive) */
  var lastEDU: Int = -1) extends Serializable {

  /** Creates a terminal DiscourseTree */
  def this(sent:Int, ft:Int, lt:Int, doc:Document, e:Int) =
    this("", RelationDirection.None, null, TreeKind.Nucleus,
      doc.sentences(sent).words.slice(ft, lt + 1).mkString(" "), (-1, -1),
      new TokenOffset(sent, ft), new TokenOffset(sent, lt),
      e, e)

  /** Creates a simple non-terminal tree */
  def this(l:String, d:RelationDirection.Value, c:Array[DiscourseTree]) =
    this(l, d, c, TreeKind.Nucleus, null, (-1, -1),
      c.head.firstToken, c.last.lastToken,
      c.head.firstEDU, c.last.lastEDU)

  def isTerminal:Boolean = children == null

  def firstSentence:Int = firstToken.sentence
  def lastSentence:Int = lastToken.sentence

  def tokenCount:Int = {
    if(isTerminal) {
      assert(firstToken.sentence == lastToken.sentence)
      lastToken.token - firstToken.token + 1
    } else {
      var sum = 0
      for(c <- children) {
        sum += c.tokenCount
      }
      sum
    }
  }

  def eduCount:Int = {
    if(isTerminal) 1
    else {
      var sum = 0
      for(c <- children) {
        sum += c.eduCount
      }
      sum
    }
  }

  override def toString:String = {
    val os = new StringBuilder
    print(os, 0, printChildren = true, printKind = false)
    os.toString()
  }
  def toString(printChildren:Boolean, printKind:Boolean):String = {
    val os = new StringBuilder
    print(os, 0, printChildren, printKind)
    os.toString()
  }
  def print(os:StringBuilder,
            offset:Int,
            printChildren:Boolean,
            printKind:Boolean) {
    var i = 0
    while(i < offset) {
      os.append(" ")
      i += 1
    }
    var printedText = false
    if(printKind) {
      os.append(kind)
      printedText = true
    }
    if(relationLabel.length > 0) {
      if(printKind) {
        os.append(":")
      }
      os.append(relationLabel)
      if(relationDirection != RelationDirection.None) {
        os.append(" (")
        os.append(relationDirection)
        os.append(")")
      }
      printedText = true
    }

    if(rawText != null) {
      if(printedText) os.append(" ")
      os.append("TEXT")
      if(charOffsets._1 != -1) {
        os.append("(")
        os.append(charOffsets._1)
        os.append(", ")
        os.append(charOffsets._2)
        os.append(")")
      }
      os.append(":")
      os.append(rawText)
    }

    if(printChildren) {
      os.append("\n")
      if(! isTerminal) {
        for(c <- children) c.print(os, offset + 2, printChildren, printKind)
      }
    }
  }


  /** Generate JSON output for the discourse parser visualizer using the default arguments. */
  def visualizerJSON (): String = {
    visualizerJSON(printChildren = true, printKind = false, pprint = false)
  }

  /**
    * Generate a JSON output string for this discourse tree, specifically including
    * only information currently needed for the discourse parser visualizer.
    */
  def visualizerJSON (printChildren:Boolean, printKind:Boolean, pprint:Boolean): String = {
    val json = buildJSON(JObject(), printChildren, printKind)
    if (pprint)
      pretty(render(json))
    else
      compact(render(json))
  }

  /** Build and return a JSON object for this discourse tree. */
  def buildJSON (argJson:JValue, printChildren:Boolean, printKind:Boolean): JValue = {
    var json = argJson

    if (printKind) {
      json = json merge JObject(JField("kind", JString(kind.toString())))
    }

    if (relationLabel.length > 0) {
      json = json merge JObject(JField("relLabel", JString(relationLabel)))
      if (relationDirection != RelationDirection.None) {
        json = json merge JObject(JField("relDir", JString(relationDirection.toString())))
      }
    }

    if (rawText != null) {
      json = json merge JObject(JField("text", JString(rawText)))
    }

    if (printChildren) {
      if (!isTerminal) {
        val kids = for (c <- children) yield c.buildJSON(JObject(), printChildren, printKind)
        if (kids.length > 0)
          json = json merge JObject(JField("kids", JArray(kids.toList)))
      }
    }
    return json
  }
}

case class TokenOffset (sentence:Int, token:Int) {
  override def toString:String = s"($sentence, $token)"
}

object TreeKind extends Enumeration {
  type TreeKind = Value
  val Nucleus, Satellite, Root = Value
}

object RelationDirection extends Enumeration {
  type RelationDirection = Value
  val LeftToRight, RightToLeft, None = Value
}
