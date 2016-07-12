package org.clulab.swirl2

import org.clulab.processors.Sentence
import org.clulab.utils.StringUtils._
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

/**
 * Some analyses of what makes a valid SRL argument
 * User: mihais
 * Date: 9/29/15
 */
object ValidCandidate {
  val logger = LoggerFactory.getLogger(classOf[ArgumentClassifier])

  val VALID_ARG_POS = "NN|IN|PR|JJ|TO|RB|VB|MD|WD|CD|\\$|WP|DT".r
  val MAX_TOKENS_BETWEEN_PRED_AND_ARG = 25
  val MAX_PATH_LEN_BETWEEN_PRED_AND_ARG = 6

  def isValid(sent:Sentence, arg:Int, pred:Int):Boolean = {
    // the POS of the argument must start with this pattern
    // if(VALID_ARG_POS.findFirstIn(sent.tags.get(arg)).isEmpty) return false

    // the number of tokens between pred and arg cannot be too large
    // if(math.abs(pred - arg) > MAX_TOKENS_BETWEEN_PRED_AND_ARG) return false

    // the dep path between predicate and argument cannot be too long
    val deps = sent.stanfordBasicDependencies.get
    val paths = deps.shortestPathEdges(pred, arg, ignoreDirection = true)
    var validPath = false
    paths.foreach(p => if(p.size < MAX_PATH_LEN_BETWEEN_PRED_AND_ARG) validPath = true)
    if(! validPath) return false

    true
  }

  def argSubpath(path:Seq[(Int, Int, String, String)]):Seq[(Int, Int, String, String)] = {
    val subpath = new ListBuffer[(Int, Int, String, String)]
    var foundAncestor = false
    for(p <- path.reverse if ! foundAncestor) {
      if(p._4 == ">") {
        subpath.insert(0, p)
      } else {
        foundAncestor = true
      }
    }
    //println(s"""Full path: ${path.mkString(", ")}""")
    //println(s"""Arg path: ${subpath.mkString(", ")}""")
    subpath.toList
  }

  def predSubpath(path:Seq[(Int, Int, String, String)]):Seq[(Int, Int, String, String)] = {
    val subpath = new ListBuffer[(Int, Int, String, String)]
    var foundAncestor = false
    for(p <- path if ! foundAncestor) {
      if(p._4 == "<") {
        subpath += p
      } else {
        foundAncestor = true
      }
    }
    subpath.toList
  }

  def main(args:Array[String]): Unit = {
    val props = argsToProperties(args)
    val path = props.getProperty("path")
    val reader = new Reader
    val doc = reader.load(path)

    // checks how many true arguments are covered by the isValid() method
    logger.debug("Started analysis...")
    var totalCands = 0
    var totalPositives = 0
    var coveredPositives = 0
    var totalCovered = 0
    for(s <- doc.sentences) {
      val outEdges = s.semanticRoles.get.outgoingEdges
      for (pred <- s.words.indices if isPred(pred, s)) {
        val args = outEdges(pred).map(_._1).toSet
        for (arg <- s.words.indices) {
          totalCands += 1

          if (args.contains(arg)) {
            totalPositives += 1
            if (isValid(s, arg, pred)) {
              coveredPositives += 1
              totalCovered += 1
            }
          } else {
            if (isValid(s, arg, pred)) {
              totalCovered += 1
            }
          }
        }
      }
    }
    logger.debug("Analysis complete.")

    val recall = coveredPositives.toDouble / totalPositives.toDouble
    val cov = totalCovered.toDouble / totalCands.toDouble
    println(s"Recall = $coveredPositives/$totalPositives = $recall")
    println(s"Total candidates = $totalCovered/$totalCands = $cov")
  }

  def isPred(position:Int, s:Sentence):Boolean = {
    val oes = s.semanticRoles.get.outgoingEdges
    position < oes.length && oes(position) != null && oes(position).nonEmpty
  }
}
