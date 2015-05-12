package edu.arizona.sista.processors

import edu.arizona.sista.struct.HashTrie
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

/**
 *
 * User: mihais
 * Date: 5/12/15
 */
class TestHashTrie extends AssertionsForJUnit {
  @Test def test1(): Unit = {
    val trie = new HashTrie()
    trie.add(Array("a", "a", "b"))
    trie.add(Array("a", "b", "b"))
    trie.add(Array("b", "b", "b"))
    trie.add(Array("a", "b", "b", "c"))
    trie.add(Array("a"))

    println("TRIE:\n" + trie)

    val tokens = Array("a", "a", "b", "d", "a", "b", "d", "b", "b", "b")
    val labels = trie.find(tokens, "E", "O")
    println("TOKENS: " + tokens.mkString(" "))
    println("LABELS: " + labels.mkString(" "))

    assertTrue(sameLabels(Array("B-E", "I-E", "I-E", "O", "B-E", "O", "O", "B-E", "I-E", "I-E"), labels))
  }

  private def sameLabels(l1:Array[String], l2:Array[String]):Boolean = {
    if(l1.length != l2.length) return false
    for(i <- 0 until l1.length)
      if(l1(i) != l2(i)) return false
    true
  }

  @Test def test2(): Unit = {
    val trie = new HashTrie()
    trie.add(Array("a", "a", "b"))
    trie.add(Array("a", "b"))
    trie.add(Array("c", "d", "e"))

    println("TRIE:\n" + trie)
    assertTrue(trie.entries.size == 2)
    assertTrue(trie.uniqueStrings.size == 5)
  }
}
