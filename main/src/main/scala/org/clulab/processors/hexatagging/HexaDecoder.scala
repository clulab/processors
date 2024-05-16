package org.clulab.processors.hexatagging

class HexaDecoder {
  def decode(
    termTags: Array[Array[(String, Float)]], 
    nonTermTags: Array[Array[(String, Float)]],
  ): BHT = {
    null
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