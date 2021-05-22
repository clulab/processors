package org.clulab.embeddings

import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.InputStreamer
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

class TestWordEmbeddingMap extends FlatSpec with Matchers {
  val name = "/test_vectors"

  behavior of "ExplicitWordEmbeddingMap"

  it should "load and cache in text format" in {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(name, compact = false)
    wordEmbeddingMap.getClass.getSimpleName should be ("ExplicitWordEmbeddingMap")

    val timeOpt = wordEmbeddingMap.get("time")
    timeOpt should be ('defined)

    val cachedWordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(name, compact = false)
    cachedWordEmbeddingMap.eq(wordEmbeddingMap) should be (true)
  }

  it should "have a unknown vector" in {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(name, compact = false)
    val map = wordEmbeddingMap.asInstanceOf[ExplicitWordEmbeddingMap]

    map.unkEmbeddingOpt should be ('defined)
    val unknown = map.unkEmbeddingOpt.get

    val timeoutOpt = map.get("timeout")
    timeoutOpt should be ('empty)
    map.getOrElseUnknown("timeout") should be (unknown)
    // TODO, see if this cached one can be returned or does it need to be copied?
    map.getOrElseUnknown("timeout").eq(unknown) should be (true)
  }

  it should "load and cache in binary format" in {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(name, compact = false)
    val map = wordEmbeddingMap.asInstanceOf[ExplicitWordEmbeddingMap]

    val binKey = "explicitWordEmbeddingMap"
    val binName = binKey + InputStreamer.binExtension
    map.save(binName)

    val binWordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(binKey, compact = false)
    binWordEmbeddingMap.getClass.getSimpleName should be ("ExplicitWordEmbeddingMap")
    new File(binName).delete()

    val cachedWordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(binKey, compact = false)
    cachedWordEmbeddingMap should be (wordEmbeddingMap)
  }

  behavior of "CompactWordEmbeddingMap"

  it should "load and cache in text format" in {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(name, compact = true)
    wordEmbeddingMap.getClass.getSimpleName should be ("CompactWordEmbeddingMap")

    val timeOpt = wordEmbeddingMap.get("time")
    timeOpt should be ('defined)

    val cachedWordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(name, compact = true)
    cachedWordEmbeddingMap.eq(wordEmbeddingMap) should be (true)
  }

  it should "have a unknown vector" in {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(name, compact = true)
    val map = wordEmbeddingMap.asInstanceOf[CompactWordEmbeddingMap]

    map.unkEmbeddingOpt should be ('defined)
    val unknown = map.unkEmbeddingOpt.get

    val timeoutOpt = map.get("timeout")
    timeoutOpt should be ('empty)
    map.getOrElseUnknown("timeout") should be (unknown)
    map.getOrElseUnknown("timeout").eq(unknown) should be (true)
  }

  it should "load and cache in binary format" in {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(name, compact = true)
    val map = wordEmbeddingMap.asInstanceOf[CompactWordEmbeddingMap]

    val binKey = "compactWordEmbeddingMap"
    val binName = binKey + InputStreamer.binExtension
    map.saveKryo(binName) // default to kryo format now

    val binWordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(binKey, compact = true)
    binWordEmbeddingMap.getClass.getSimpleName should be ("CompactWordEmbeddingMap")
    new File(binName).delete

    val cachedWordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(binKey, compact = true)
    cachedWordEmbeddingMap should be (wordEmbeddingMap)
  }

  behavior of "the quadruplet"

  it should "be identical" in {
    val keysAndCompacts = Seq(
      (name, false),
      (name, true),
      ("explicitWordEmbeddingMap", false),
      ("compactWordEmbeddingMap", true)
    )
    val wordEmbeddingMaps = keysAndCompacts.map { case (key, compact) => WordEmbeddingMapPool.getOrElseCreate(key, compact) }

    wordEmbeddingMaps.foreach { wordEmbeddingMap =>
      wordEmbeddingMap.get("time") should be ('defined)
      wordEmbeddingMap.get("timeout") should not be ('defined)

      wordEmbeddingMap.isOutOfVocabulary("time") should be (false)
      wordEmbeddingMap.isOutOfVocabulary("timeout") should be (true)
      wordEmbeddingMap.isOutOfVocabulary("") should be (true)
    }

    def resultsMatch[T](expected: WordEmbeddingMap, actuals: Seq[WordEmbeddingMap])(f: WordEmbeddingMap => T): Unit = {
      val expectedResult = f(expected)

      actuals.foreach { actual =>
        val actualResult = f(actual)

        actualResult should be (expectedResult)
      }
    }

    val expected = wordEmbeddingMaps.head
    val actuals = wordEmbeddingMaps.tail

    resultsMatch(expected, actuals) { wordEmbeddingMap => wordEmbeddingMap.makeCompositeVector(Seq("Once", "upon", "a", "time")) }
    resultsMatch(expected, actuals) { wordEmbeddingMap => wordEmbeddingMap.makeCompositeVectorWeighted(Seq("Once", "upon", "a", "time"), Seq(1f, 2f, 3f, 4f)) }
    resultsMatch(expected, actuals) { wordEmbeddingMap => wordEmbeddingMap.avgSimilarity(Seq("Once", "upon", "a", "time"), Seq("Any", "time", "at", "all")) }
  }

  behavior of "glove"

  it should "load faster serialized" in {
    val fileName = "../glove.840B.300d.10f"
    val resourceName = "/org/clulab/glove/glove.840B.300d.10f"

    if (false) {
      // Copy the text resource to a local file.
      val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(fileName, compact = true)
      wordEmbeddingMap.save(fileName + InputStreamer.binExtension)
    }

    {
      // Race from file system
      if (false) {
        val start = System.currentTimeMillis()
        val inputStreamer = new InputStreamer()
        val inputStream = inputStreamer.getFileAsStream(fileName + InputStreamer.txtExtension)
        val glove = inputStream.autoClose { inputStream =>
          ExplicitWordEmbeddingMap(inputStream, false)
        }
        val stop = System.currentTimeMillis()
        val elapsed = stop - start
        println(s"Elapsed time for txt version: $elapsed")
      }

      if (false) {
        val start = System.currentTimeMillis()
        val inputStreamer = new InputStreamer()
        val inputStream = inputStreamer.getFileAsStream(fileName + InputStreamer.binExtension)
        val glove = inputStream.autoClose { inputStream =>
          CompactWordEmbeddingMap(inputStream, true)
        }
        val stop = System.currentTimeMillis()
        val elapsed = stop - start
        println(s"Elapsed time for bin version: $elapsed")
      }
    }

    // Race from jar
    if (false) {
      val start = System.currentTimeMillis()
      val inputStreamer = new InputStreamer()
      val inputStream = inputStreamer.getResourceAsStream(resourceName + InputStreamer.txtExtension)
      val glove = inputStream.autoClose { inputStream =>
        ExplicitWordEmbeddingMap(inputStream, false)
      }
      val stop = System.currentTimeMillis()
      val elapsed = stop - start
      println(s"Elapsed time for txt version: $elapsed")
    }

    if (false) {
      val start = System.currentTimeMillis()
      val inputStreamer = new InputStreamer()
      val inputStream = inputStreamer.getResourceAsStream(resourceName + InputStreamer.binExtension)
      val glove = inputStream.autoClose { inputStream =>
        CompactWordEmbeddingMap(inputStream, true)
      }
      val stop = System.currentTimeMillis()
      val elapsed = stop - start
      println(s"Elapsed time for bin version: $elapsed")
    }
  }
}
