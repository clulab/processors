package org.clulab.processors.hexatagging

import scala.collection.mutable.Stack

class HexaDecoder {
  def decode(
    termTags: Array[Array[(String, Float)]], 
    nonTermTags: Array[Array[(String, Float)]]
  ): BHT = {
    val stack = new Stack[BHT]
    decode(stack, termTags, nonTermTags)
    val bht = stack.pop()
    bht
  }

  def decode(
    stack: Stack[BHT], 
    termTags: Array[Array[(String, Float)]], // assumes sorted in descending order of scores
    nonTermTags: Array[Array[(String, Float)]] // assumes sorted in descending order of scores
  ): Unit = {
    assert(termTags.length > 1) // this decoder assumes at least 2 words in the sentence
    assert(termTags.length == nonTermTags.length)
    for(i <- termTags.indices) {
      //
      // 1. first process the current terminal tag
      //
      val termTag: String = 
        // if the stack is empty, we can't use a TERMINAL_RIGHT_CHILD
        if(stack.isEmpty) {
          val tagOpt = termTags(i).find(_._1.startsWith(HexaTags.TERMINAL_LEFT_CHILD))
          if(tagOpt.isEmpty) {
            throw new RuntimeException(s"ERROR: expected a ${HexaTags.TERMINAL_LEFT_CHILD}!")
          }
          tagOpt.get._1
        } else {
          termTags(i).head._1
        }
      
      if(termTag.startsWith(HexaTags.TERMINAL_LEFT_CHILD)) {
        // shift the leaf node into the stack
        val bht = new TerminalBHT(i, termTag.substring(HexaTags.TERMINAL_LEFT_CHILD.length + 1))
        println(s"Pushing $bht onto the stack.")
        stack.push(bht)
      } else if(termTag.startsWith(HexaTags.TERMINAL_RIGHT_CHILD)) {
        // pop the subtree on the top of the stack
        // replace the dummy node in the subtree with the terminal node
        // push the subtree back to the stack
        val top = stack.pop()
        assert(! top.isTerminal)
        val nonTerm = top.asInstanceOf[NonTerminalBHT]
        val term = new TerminalBHT(i, termTag.substring(HexaTags.TERMINAL_RIGHT_CHILD.length + 1))
        if(nonTerm.left == null) {
          nonTerm.left = term
        } else if(nonTerm.right == null) {
          nonTerm.right = term
        } else {
          throw new RuntimeException(s"ERROR: expecting a nonTerm with one dummy child!")
        }
        stack.push(nonTerm)
      } else {
        throw new RuntimeException(s"ERROR: unknown terminal tag $termTag!")
      }

      //
      // 2. then process the next non-terminal tag, unless we reached eos
      //
      if(i < termTags.length - 1) {

      }
    }
    assert(stack.size == 1) // must have 1 element on the stack at the end
  }
}

object HexaDecoder {
  def main(args: Array[String]) {
    // She reads fascinating papers
    val termTags = Array(
      Array(("tl-nsubj", 1.0f)),
      Array(("tr-root", 1.0f)),
      Array(("tl-nmod", 1.0f)),
      Array(("tr-dobj", 1.0f))
    )
    val nonTermTags = Array(
      Array(("nl-R", 1.0f)),
      Array(("nl-L", 1.0f)),
      Array(("nr-R", 1.0f)),
      Array(("eos", 1.0f))
    )
    val decoder = new HexaDecoder()
    val bht = decoder.decode(termTags, nonTermTags)
    println(bht)
  }
}