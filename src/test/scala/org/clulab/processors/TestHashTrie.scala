package org.clulab.processors

import org.clulab.struct.HashTrie
import org.scalatest._

/**
 *
 * User: mihais
 * Date: 5/12/15
 */
class TestHashTrie extends FlatSpec with Matchers {
  "the trie" should "label the text correctly with BIO labels" in {
    val trie = new HashTrie()
    trie.add(Array("a", "a", "b"))
    trie.add(Array("a", "b", "b"))
    trie.add(Array("b", "b", "b"))
    trie.add(Array("a", "b", "b", "c"))
    trie.add(Array("a"))

    //println("TRIE:\n" + trie)

    val tokens = Array("a", "a", "b", "d", "a", "b", "d", "b", "b", "b")
    val labels = trie.find(tokens, "E", "O")
    //println("TOKENS: " + tokens.mkString(" "))
    //println("LABELS: " + labels.mkString(" "))

    sameLabels(Array("B-E", "I-E", "I-E", "O", "B-E", "O", "O", "B-E", "I-E", "I-E"), labels) should be (true)
  }

  "the trie" should "have 2 entries and contain 5 unique strings" in {
    val trie = new HashTrie()
    trie.add(Array("a", "a", "b"))
    trie.add(Array("a", "b"))
    trie.add(Array("c", "d", "e"))

    //println("TRIE:\n" + trie)
    trie.entries.size should be (2)
    trie.uniqueStrings.size should be (5)
  }

  private def sameLabels(l1:Array[String], l2:Array[String]):Boolean = {
    if(l1.length != l2.length) return false
    for(i <- 0 until l1.length)
      if(l1(i) != l2(i)) return false
    true
  }
}
