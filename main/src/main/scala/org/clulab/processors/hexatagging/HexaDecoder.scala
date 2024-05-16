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
    termTags: Array[Array[(String, Float)]], 
    nonTermTags: Array[Array[(String, Float)]]
  ): Unit = {
    assert(termTags.length > 1) // this decoder assumes at least 2 words in the sentence
    assert(termTags.length == nonTermTags.length)
    for(i <- termTags.indices) {
      // first process the current terminal tag

      // then process the next non-terminal tag
    }
    assert(stack.size == 1) // must have 1 element on the stack at the end
  }
}

object HexaDecoder {
  def main(args: Array[String]) {
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