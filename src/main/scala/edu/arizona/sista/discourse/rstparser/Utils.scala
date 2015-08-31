package edu.arizona.sista.discourse.rstparser

import edu.arizona.sista.learning._
import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.struct.{DirectedGraph, Lexicon, Tree}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.mutable

/**
 * Various utils useful for feature generation, etc.
 * User: mihais
 * Date: 5/29/14
 */
object Utils {
  /**
   * Fetches dependencies from a given sentence
   * Note: RST parsing works better with basic dependencies, so we ALWAYS use them here!
   */
  def deps(s:Sentence):DirectedGraph[String] = s.stanfordBasicDependencies.get

  def hasDeps(s:Sentence):Boolean = s.stanfordBasicDependencies.isDefined

  def tokenCount(doc:Document) = {
    var sum = 0
    for(s <- doc.sentences)
      sum += s.size
    sum
  }

  def extractConnectives(tree:DiscourseTree, doc:Document):Counter[String] = {
    val words = extractWords(tree, doc, words = null)
    val c = new Counter[String]()
    for(offset <- 0 until words.size) {
      val conn = ConnectiveMatcher.matchesConnective(words, offset)
      if(conn != null) {
        c.incrementCount(mkDiscretePosition(offset, words.size) + ":" + conn.mkString("_"))
      }
    }
    c
  }

  def mkDiscretePosition(offset:Int, total:Int):String = {
    if(offset < total.toDouble * 1.0/3.0) return "B"
    if(offset < total.toDouble * 2.0/3.0) return "M"
    "E"
  }

  def sameSubtree(t1:Tree, t2:Tree) =
    if(t1 != null && t2 != null && t1.startOffset == t2.startOffset && t1.endOffset == t2.endOffset) true else false

  /** Finds the smallest subtree with label S* that includes this discourse fragment */
  def findSubtree(tree:DiscourseTree, doc:Document):Tree = {
    // this only makes sense if fragment in a single sentence
    if(tree.firstSentence != tree.lastSentence) return null
    // this can only be done if syntactic analysis provided
    if(! doc.sentences(tree.firstSentence).syntacticTree.isDefined) return null

    val start = tree.firstToken.token
    val end = tree.lastToken.token + 1 // DiscourseTree is inclusive but Processor is not!
    val subtree = findSubtree(doc.sentences(tree.firstSentence).syntacticTree.get, start, end)

    //println(s"found subtree ($start, $end):\n" + subtree)
    //println("full tree is:\n" + doc.sentences(tree.firstSentence).syntacticTree.get)

    subtree
  }

  /** Finds the smallest subtree with label S* that includes this span */
  def findSubtree(tree:Tree, start:Int, end:Int):Tree = {
    if(! tree.isLeaf) {
      var m:Tree = null
      for (c <- tree.children.get if m == null) {
        m = findSubtree(c, start, end)
      }
      if (m != null) return m
    }

    if(! tree.isLeaf && tree.value.startsWith("S") && tree.startOffset <= start && tree.endOffset >= end) tree
    else null
  }

  def extractWords(tree:DiscourseTree,
                   doc:Document,
                   words:Counter[String] = null,
                   threshold:Int = 1000):Array[String] = {
    val b = new ListBuffer[String]
    var s = tree.firstSentence
    var t = tree.firstToken.token
    while(s <= tree.lastSentence) {
      while((s < tree.lastSentence && t < doc.sentences(s).size) ||
        (s == tree.lastSentence && t <= tree.lastToken.token)) {
        val w = doc.sentences(s).words(t)
        if(words == null || words.getCount(w) > threshold)
          b += w
        t += 1
      }
      s += 1
      t = 0
    }
    b.toArray
  }

  def extractTags(tree:DiscourseTree,
                  doc:Document):Array[String] = {
    val b = new ListBuffer[String]
    var s = tree.firstSentence
    var t = tree.firstToken.token
    while(s <= tree.lastSentence) {
      while((s < tree.lastSentence && t < doc.sentences(s).size) ||
        (s == tree.lastSentence && t <= tree.lastToken.token)) {
        val tag = doc.sentences(s).tags.get(t)
        b += tag
        t += 1
      }
      s += 1
      t = 0
    }
    b.toArray
  }

  def extractRightMost(t:DiscourseTree, nodes:ArrayBuffer[DiscourseTree]) {
    nodes += t
    if(! t.isTerminal)
      extractRightMost(t.children.last, nodes)
  }

  def extractLeftMost(t:DiscourseTree, nodes:ArrayBuffer[DiscourseTree]) {
    nodes += t
    if(! t.isTerminal)
      extractLeftMost(t.children.head, nodes)
  }

  def countWords(trees:List[(DiscourseTree, Document)]):Counter[String] = {
    val c = new Counter[String]
    for(td <- trees) {
      for(s <- td._2.sentences) {
        for(w <- s.words) {
          c.incrementCount(w)
        }
      }
    }
    c
  }

  def countNgrams(trees:List[(DiscourseTree, Document)]):Counter[String] = {
    val c = new Counter[String]
    for(td <- trees) {
      val words = extractWords(td._1, td._2)
      c.incrementCount(mkNgram(words, 0, 3))
      c.incrementCount(mkNgram(words, words.size - 3, words.size))
    }
    c
  }

  def mkNgram(words:Array[String], start:Int, end:Int):String = {
    val b = new mutable.StringBuilder()
    var first = true
    for(i <- math.max(start, 0) until math.min(end, words.size)) {
      if(! first) b.append("-")
      b.append(words(i))
      first = false
    }
    b.toString()
  }

  def findSyntacticParentWithRightSibling(
                          root:Tree,
                          position:Int,
                          parent:Tree = null,
                          right:Tree = null):(Tree, Tree, Tree) = {
    //println("inspecting tree " + root)
    if(root.headOffset == position && right != null) {
      return new Tuple3(root, parent, right)
    }

    if(! root.isLeaf) {
      assert(root.children.isDefined)
      //println(s"found ${root.children.get.length} children.")
      for(i <- 0 until root.children.get.length) {
        val c = root.children.get(i)
        var r:Tree = null
        if(i < root.children.get.length - 1) r = root.children.get(i + 1)
        val v = findSyntacticParentWithRightSibling(c, position, root, r)
        if(v._1 != null) return v
      }
    }

    (null, null, null)
  }

  def findSyntacticParent(
    root:Tree,
    position:Int,
    parent:Tree = null):(Tree, Tree) = {
    //println("inspecting tree " + root)
    if(root.headOffset == position) {
      return new Tuple2(root, parent)
    }

    if(! root.isLeaf) {
      assert(root.children.isDefined)
      //println(s"found ${root.children.get.length} children.")
      for(i <- 0 until root.children.get.length) {
        val c = root.children.get(i)
        val v = findSyntacticParent(c, position, root)
        if(v._1 != null) return v
      }
    }

    (null, null)
  }

  def findSyntacticHeadFromDependencies( deps:DirectedGraph[String],
                                         first:Int,
                                         last:Int): (Int, Int, String) = {
    // the head of this span is the token that is a root, or
    // the head of this span is the left-most token whose syntactic head falls outside the given span
    for(i <- first to last) {
      if(deps.roots.contains(i)) { // found a root
        return (i, -1, "")
      }

      val heads = deps.incomingEdges(i)
      var outside = false
      var p = -1
      var l = ""
      for(h <- heads if ! outside) {
        if(h._1 < first || h._1 > last) {
          outside = true
          p = h._1
          l = h._2
        }
      }
      if(outside) { // found the head
        return (i, p, l)
      }
    }

    (-1, -1, "")
  }


  def findSyntacticHead( root:Tree,
                         parent:Tree,
                         first:Int,
                         last:Int): (Tree, Tree) = {
    if(root.headOffset >= first && root.headOffset <= last) {
      return new Tuple2(root, parent)
    }

    if(! root.isLeaf) {
      assert(root.children.isDefined)
      for(c <- root.children.get) {
        val r = findSyntacticHead(c, root, first, last)
        if(r._1 != null) return r
      }
    }

    (null, null)
  }

  def findSmallestCommonAncestor( root:Tree,
                                  first:Int,
                                  last:Int): Tree = {
    if(! root.isLeaf) {
      assert(root.children.isDefined)
      for(c <- root.children.get) {
        val r = findSmallestCommonAncestor(c, first, last)
        if(r != null) return r
      }
    }

    if(root.startOffset <= first && root.endOffset >= last) {
      return root
    }

    null
  }

  def findCommonAncestorsFromDependencies( deps:DirectedGraph[String],
                                           first:Int,
                                           last:Int): Iterable[Int] = {
    val ancestors = new ListBuffer[Int]
    for(i <- first to last) {
      if(deps.roots.contains(i)) { // found a root
        ancestors += -1
      }

      assert(i >= 0)
      if(i < deps.incomingEdges.size) {
        // i >= size may happen for corenlp; not sure why
        val heads = deps.incomingEdges(i)
        if (heads != null) {
          for (h <- heads) {
            if (h._1 < first || h._1 > last) {
              ancestors += h._1
            }
          }
        }
      }
    }
    ancestors.toList
  }

  def toDecile(v:Int, max:Int):Int = {
    val dec = 10.0 * v.toDouble / max.toDouble
    for(i <- 1 to 10) {
      if(dec < i) return i.toInt
    }
    1
  }

  def prefix(f:String, sep:String):String = {
    var pref = f
    val i = f.indexOf(sep)
    if(i > 0) pref = f.substring(0, i)
    pref
  }

  def findFeatureGroups(sep:String, lexicon:Lexicon[String]):Map[String, Set[Int]] = {
    val groups = new mutable.HashMap[String, mutable.HashSet[Int]]()
    for(f <- lexicon.keySet) {
      val pref = prefix(f, sep)

      if(! groups.contains(pref))
        groups.put(pref, new mutable.HashSet[Int]())
      groups.get(pref).get += lexicon.get(f).get
    }

    val img = new mutable.HashMap[String, Set[Int]]()
    for(k <- groups.keySet) {
      img.put(k, groups.get(k).get.toSet)
    }
    img.toMap
  }

  def svmFactory():Classifier[String, String] = new LinearSVMClassifier[String, String]()

  def lrFactory():Classifier[String, String] = new LogisticRegressionClassifier[String, String]()

  def perceptronFactory():Classifier[String, String] = new PerceptronClassifier[String, String]()

  def printTopWeights(c:LiblinearClassifier[String, String]) {
    val ws = c.getWeights()
    for(l <- ws.keySet) {
      println(s"Top weights for label $l:")
      val w = ws.get(l).get.sorted.toArray
      for(i <- 0 until math.min(w.size, 10)) {
        println(s"\t${w(i)._1} ${w(i)._2}")
      }
    }
  }

  /**
   * Generates a compact description of the EDUs in this document, using a formalism similar to Feng and Hirst (2012)
   * @param tree Discourse tree for this document
   * @return Format: one sentence per element; each sentence stores an array of EDUs; end token offsets are inclusive!
   */
  def mkGoldEDUs(tree:DiscourseTree, doc:Document):Array[Array[(Int, Int)]] = {
    val eduBuffer = new Array[ArrayBuffer[(Int, Int)]](doc.sentences.size)
    for(i <- 0 until eduBuffer.size) eduBuffer(i) = new ArrayBuffer[(Int, Int)]()
    addGoldEDUs(tree, eduBuffer)
    val edus = new Array[Array[(Int, Int)]](doc.sentences.size)
    for(i <- 0 until eduBuffer.size)
      edus(i) = eduBuffer(i).toArray
    edus
  }

  private def addGoldEDUs(tree:DiscourseTree, edus:Array[ArrayBuffer[(Int, Int)]]) {
    if(tree.isTerminal) {
      assert(tree.firstSentence == tree.lastSentence)
      val s = tree.firstSentence
      val f = tree.firstToken.token
      val l = tree.lastToken.token
      // store the span of this EDU. Last token is inclusive!
      edus(s) += new Tuple2(f, l)
      // store the position of this EDU in the tree
      tree.firstEDU = edus(s).size - 1
      tree.lastEDU = edus(s).size - 1 // this is also inclusive!
    } else {
      for(c <- tree.children) {
        addGoldEDUs(c, edus)
      }

      // store EDU positions in non-terminal trees as well
      tree.firstEDU = tree.children.head.firstEDU
      tree.lastEDU = tree.children.last.lastEDU
    }
  }
}
