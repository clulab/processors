package org.clulab.hexatagging

import org.clulab.processors.hexatagging._
import org.clulab.utils.Test
import org.clulab.struct.Edge

class HexaDecoderTest extends Test {
  val decoder: HexaDecoder = new HexaDecoder()

  "HexaDecoder" should "decode a simple tree" in {
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

    val (bht, deps, roots) = decoder.decode(termTags, nonTermTags, 10)

    println(bht)
    println(s"Dependencies (${deps.size}):")
    println(deps.mkString("\n"))
    println("Roots: " + roots.mkString(", "))

    roots.size should be (1)
    roots.toList.head should be (1)
    val depArray = deps.toArray
    depArray.length should be (3)
    depArray(0) should be (new Edge[String](1, 3, "dobj"))
    depArray(1) should be (new Edge[String](1, 0, "nsubj"))
    depArray(2) should be (new Edge[String](3, 2, "nmod"))
  }

  it should "decode another simple tree" in {
    // this is a test
    val termTags = Array(
      Array(("tl-nsubj", 1.0f)),
      Array(("tl-cop", 1.0f)),
      Array(("tl-det", 1.0f)),
      Array(("tr-root", 1.0f))
    )
    val nonTermTags = Array(
      Array(("nl-R", 1.0f)),
      Array(("nr-R", 1.0f)),
      Array(("nr-R", 1.0f)),
      Array(("eos", 1.0f))
    )

    val (bht, deps, roots) = decoder.decode(termTags, nonTermTags, 10, verbose=true)

    println(bht)
    println(s"Dependencies (${deps.size}):")
    println(deps.mkString("\n"))
    println("Roots: " + roots.mkString(", "))

    roots.size should be (1)
    roots.toList.head should be (3)
    val depArray = deps.toArray
    depArray.length should be (3)
    depArray(0) should be (new Edge[String](3, 0, "nsubj"))
    depArray(1) should be (new Edge[String](3, 1, "cop"))
    depArray(2) should be (new Edge[String](3, 2, "det"))
  }
}
