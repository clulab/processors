package edu.arizona.sista.processor.struct

import scala.collection.mutable.ListBuffer
import edu.arizona.sista.processor.ling.TreebankLabels

/**
 * Tree utilities
 * User: mihais
 * Date: 4/23/13
 */
object Trees {

  def findBaseNounPhrases(root:Tree[String]):List[Tree[String]] = {
    val bnpBuffer = new ListBuffer[Tree[String]]
    findBaseNounPhrases(root, bnpBuffer)
    bnpBuffer.toList
  }

  /** Returns true if this tree contains any base NPs */
  private def findBaseNounPhrases(root: Tree[String], bnps: ListBuffer[Tree[String]]): Boolean = {
    var hasInnerBaseNPs = false
    root.children.foreach(_.foreach(c => {
      if(findBaseNounPhrases(c, bnps) == true) {
        hasInnerBaseNPs = true
      }
    }))

    var isBaseNP = false
    if(root.value == TreebankLabels.NP && ! hasInnerBaseNPs) {
      bnps += root
      isBaseNP = true
    }

    (isBaseNP || hasInnerBaseNPs)
  }
}
