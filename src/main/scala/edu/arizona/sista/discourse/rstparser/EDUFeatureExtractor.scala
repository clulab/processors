package edu.arizona.sista.discourse.rstparser

import edu.arizona.sista.processors.{Sentence, Document}
import org.slf4j.LoggerFactory
import EDUFeatureExtractor._
import scala.collection.mutable
import edu.arizona.sista.struct.Counter

/**
 * Generates features necessary for EDU classification
 * User: mihais
 * Date: 5/21/14
 */
class EDUFeatureExtractor {
  var features:Option[Counter[String]] = None

  def f(fn:String, fv:Double = 1) {
    assert(features.isDefined)
    features.get.incrementCount(fn, fv)
  }

  def mkFeatures(position:TokenOffset,
                 doc:Document,
                 connectives:Array[Array[String]]):Counter[String] = {
    features = Some(new Counter[String])
    val sent = doc.sentences(position.sentence)
    val cs = connectives(position.sentence)

    //
    // word features
    //
    for(i <- EDU_LEFT_CTX to EDU_RIGHT_CTX) {
      val word = getWord(sent, position.token + i)
      val nextWord = getWord(sent, position.token + i + 1)
      val nextNextWord = getWord(sent, position.token + i + 2)
      f("w" + i + ":" + word)
      f("2xw" + i + ":" + word + "|" + nextWord)
      f("3xw" + i + ":" + word + "|" + nextWord + "|" + nextNextWord)
    }

    //
    // POS features
    //
    if(sent.tags.isDefined) {
      for(i <- EDU_LEFT_CTX to EDU_RIGHT_CTX) {
        val pos = getTag(sent, position.token + i)
        val nextPos = getTag(sent, position.token + i + 1)
        val nextNextPos = getTag(sent, position.token + i + 2)
        f("p" + i + ":" + pos)
        f("2xp" + i + ":" + pos + "|" + nextPos)
        f("3xp" + i + ":" + pos + "|" + nextPos + "|" + nextNextPos)
      }
    }

    //
    // dependency features
    //
    if(sent.dependencies.isDefined) {
      for(i <- EDU_LEFT_CTX to EDU_RIGHT_CTX) {
        val offset = position.token + i
        val conn = getConnective(cs, offset)

        // dependencies where this token is the modifier
        val in = getIncomingDependencies(sent, offset)
        val nextIn = getIncomingDependencies(sent, offset + 1)

        if(in.size == 0) {
          f("noind" + i)
        } else {
          for (d <- in) {
            f("ind" + i + ":" + d._2)
            f("cind" + i + ":" + conn + "|" + d._2)
          }
        }

        // bigrams of dependencies where tokens are modifiers
        for(d1 <- in) {
          for(d2 <- nextIn) {
            f("2xind" + i + ":" + d1._2 + "|" + d2._2)
            f("2xcind" + i + ":" + conn + "|" + d1._2 + "|" + d2._2)
          }
        }

        // dependencies where this token is the head
        val out = getOutgoingDependencies(sent, offset)
        val nextOut = getOutgoingDependencies(sent, offset + 1)

        if(out.size == 0) {
          f("nooutd" + i)
        } else {
          for (d <- out) {
            f("outd" + i + ":" + d._2)
            f("coutd" + i + ":" + conn + "|" + d._2)
          }
        }

        // bigrams of dependencies where tokens are heads
        for(d1 <- out) {
          for(d2 <- nextOut) {
            f("2xoutd" + i + ":" + d1._2 + "|" + d2._2)
          }
        }

        // path to root
        if(offset >= 0 && offset < sent.size) {
          val (path, _) = pathToRoot(offset, sent.dependencies.get.incomingEdges)
          f("path" + i + ":" + path)
        }

        // head word for this token
        val head = getHeadWord(sent, offset)
        val word = getWord(sent, offset)
        f("hw" + i + ":" + head + "|" + word)

        // right sibling for this token
        val rs = getRightSibling(sent, offset)
        f("rs" + i + ":" + rs + "|" + word)
      }

      if(position.token < sent.dependencies.get.outgoingEdges.size) {
        val leftMost = sent.dependencies.get.outgoingEdges(position.token).size == 0
        if (leftMost) {
          val (path, top) = pathToRoot(position.token, sent.dependencies.get.incomingEdges)
          val word = getWord(sent, position.token)
          f("leftmostpath:" + path + "|" + leftMost)
          f("leftmosttop:" + top + "|" + leftMost)
          f("leftmostw:" + word + "|" + leftMost)
        }
      }
    }

    //
    // constituent features
    //
    if(sent.syntacticTree.isDefined) {
      val tree = doc.sentences(position.sentence).syntacticTree.get

      for(i <- EDU_LEFT_CTX to EDU_RIGHT_CTX) {
        val offset = position.token + i
        //println("Finding syntactic parents for token #" + position.token + " in tree:\n" + tree)
        val (nw, np, nr) = Utils.findSyntacticParentWithRightSibling(tree, offset)
        if (nw != null) {
          f("nw" + i + ":" + nw.value)
          f("nww" + i + ":" + nw.value + getWord(sent, offset))
          //println(s"NW #${nw.findHeadPosition}: " + nw)
          if (np != null) {
            //println(s"NP #${np.findHeadPosition}: " + np)
            f("np" + i + ":" + np.value)
            f("npw" + i + ":" + np.value + getWord(sent, offset))
          }
          if (nr != null) {
            //println(s"NR #${nr.findHeadPosition}: " + nr)
            f("nr" + i + ":" + nr.value)
            f("nrw" + i + ":" + nr.value + getWord(sent, nr.headOffset))
          }
        }

        val (tnw, tnp) = Utils.findSyntacticParent(tree, offset)
        if(tnw != null && (nw == null || tnw.value != nw.value)) {
          f("tnw" + i + ":" + tnw.value)
          f("tnww" + i + ":" + tnw.value + getWord(sent, offset))

          if(tnp != null && (np == null || tnp.value != np.value)) {
            f("tnp" + i + ":" + tnp.value)
            f("tnpw" + i + ":" + tnp.value + getWord(sent, offset))
          }
        }
      }
    }

    features.get
  }

  private def getOutgoingDependencies(sent:Sentence, offset:Int):Array[(Int, String)] = {
    if(offset >= 0 && offset < sent.dependencies.get.outgoingEdges.size) sent.dependencies.get.getOutgoingEdges(offset)
    else new Array[(Int, String)](0)
  }

  private def getIncomingDependencies(sent:Sentence, offset:Int):Array[(Int, String)] = {
    if(offset >= 0 && offset < sent.dependencies.get.incomingEdges.size) sent.dependencies.get.getIncomingEdges(offset)
    else new Array[(Int, String)](0)
  }


  def getRightSibling(sent:Sentence, offset:Int):String = {
    val dg = sent.dependencies.get
    if(offset >= 0 && offset < sent.size && offset < dg.size && dg.getIncomingEdges(offset).size > 0) {
      val head = sent.dependencies.get.getIncomingEdges(offset)(0)._1
      val siblings = getOutgoingDependencies(sent, head)
      var rs = Int.MaxValue
      for(s <- siblings) {
        if(s._1 > offset && s._1 < rs) {
          rs = s._1
        }
      }
      if(rs > offset && rs < sent.size) {
        return sent.words(rs)
      }
    }
    EDU_PAD
  }

  private def getHeadWord(sent:Sentence, offset:Int):String = {
    val dg = sent.dependencies.get
    if(offset < 0) EDU_PAD
    else if(offset < sent.size) {
      if(offset >= dg.incomingEdges.size || dg.getIncomingEdges(offset).size == 0) {
        EDU_PAD
      } else {
        val headPos = sent.dependencies.get.getIncomingEdges(offset)(0)._1
        if (headPos < 0) "ROOT"
        else sent.words(headPos)
      }
    }
    else EDU_PAD
  }

  private def getWord(sent:Sentence, offset:Int):String = {
    if(offset < 0) EDU_PAD
    else if(offset < sent.size) sent.words(offset)
    else EDU_PAD
  }

  private def getTag(sent:Sentence, offset:Int):String = {
    if(offset < 0) EDU_PAD
    else if(offset < sent.size) sent.tags.get(offset)
    else EDU_PAD
  }

  private def getConnective(connectives:Array[String], offset:Int):String = {
    if(offset < 0) EDU_PAD
    else if(offset < connectives.size) connectives(offset)
    else EDU_PAD
  }

  def pathToRoot(start:Int, in:Array[Array[(Int, String)]]):(String, String) = {
    val os = new StringBuilder
    var root = false
    var pos = start
    var top:String = ""
    // CoreNLP may have cycles...
    val seen = new mutable.HashSet[Int]()
    while(! root) {
      if(pos >=in.size || in(pos).size == 0) {
        root = true
      } else if(seen.contains(pos)) { // found a cycle; stop here
        os.append("CYCLE")
        root = true
      } else {
        seen.add(pos)
        val d = in(pos)(0)._2
        os.append(d)
        top = d
        os.append(" ")
        pos = in(pos)(0)._1
      }
    }
    (os.toString().trim, top)
  }

}

object EDUFeatureExtractor {
  val logger = LoggerFactory.getLogger(classOf[EDUFeatureExtractor])

  val EDU_LEFT_CTX = -3
  val EDU_RIGHT_CTX = 1

  val EDU_PAD = "PAD"

  def load(fs:String):Set[String] = {
    val features = new mutable.HashSet[String]()
    fs.split(", ").foreach(features += _.trim)
    logger.debug(s"Loaded ${features.size} feature groups for EDU segmentation.")
    features.toSet
  }
}
