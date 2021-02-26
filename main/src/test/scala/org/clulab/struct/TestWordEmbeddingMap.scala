package org.clulab.struct

import org.clulab.embeddings.CompactWordEmbeddingMap
import org.clulab.embeddings.ExplicitWordEmbeddingMap
import org.clulab.embeddings.WordEmbeddingMapPool
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestWordEmbeddingMap extends FlatSpec with Matchers {

  behavior of "ExplicitWordEmbeddingMap"

  it should "load and cache in text format" in {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate("./test_vectors", compact = false)
    wordEmbeddingMap.getClass.getSimpleName should be ("ExplicitWordEmbeddingMap")

    val timeOpt = wordEmbeddingMap.get("time")
    timeOpt should be ('defined)

    val cachedWordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate("./test_vectors", compact = false)
    cachedWordEmbeddingMap.eq(wordEmbeddingMap) should be (true)
  }

  it should "have a unknown vector" in {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate("./test_vectors", compact = false)
    val map = wordEmbeddingMap.asInstanceOf[ExplicitWordEmbeddingMap]

    map.unkEmbeddingOpt should be ('defined)
    val unknown = map.unkEmbeddingOpt.get

    val timeoutOpt = map.get("timeout")
    timeoutOpt should be ('empty)
    map.getOrElseUnknown("timeout") should be (unknown)
    // TODO, see if this cached one can be returned or does it need to be copied?
    map.getOrElseUnknown("timeout").eq(unknown) should be (true)
  }

  it should "load in binary format" in {

  }

  behavior of "CompactWordEmbeddingMap"

  it should "load and cache in text format" in {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate("./test_vectors", compact = true)
    wordEmbeddingMap.getClass.getSimpleName should be ("CompactWordEmbeddingMap")

    val timeOpt = wordEmbeddingMap.get("time")
    timeOpt should be ('defined)

    val cachedWordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate("./test_vectors", compact = true)
    cachedWordEmbeddingMap.eq(wordEmbeddingMap) should be (true)
  }

  it should "have a unknown vector" in {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate("./test_vectors", compact = true)
    val map = wordEmbeddingMap.asInstanceOf[CompactWordEmbeddingMap]

    map.unkEmbeddingOpt should be ('defined)
    val unknown = map.unkEmbeddingOpt.get

    val timeoutOpt = map.get("timeout")
    timeoutOpt should be ('empty)
    map.getOrElseUnknown("timeout") should be (unknown)
    // TODO, see if this cached one can be returned or does it need to be copied?
    map.getOrElseUnknown("timeout").eq(unknown) should be (true)
  }

  it should "load in binary format" in {

  }

  behavior of "the pair"

  it should "be identical" in {

  }
}
