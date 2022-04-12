package org.clulab.embeddings

import org.clulab.utils.Sourcer
import java.io.File

import org.scalatest._

class TestCompactWordEmbeddingMap extends FlatSpec with Matchers {

  protected def matches(array1: IndexedSeq[Float], array2: IndexedSeq[Double], epsilon: Double): Boolean = {
    array1.zip(array2).forall { case (value1, value2) =>
      math.abs(value1 - value2) < epsilon
    }
  }

  protected def matches(array1: IndexedSeq[Float], array2: IndexedSeq[Float]): Boolean = {
    array1.zip(array2).forall { case (value1, value2) =>
      math.abs(value1 - value2) == 0
    }
  }

  val filename = "/test_vectors_sanitized.txt"
  val fullsizeText = new SanitizedWordEmbeddingMap(Sourcer.sourceFromResource(filename), None, false)
  val compactText: CompactWordEmbeddingMap = CompactWordEmbeddingMap(filename, resource = true, cached = false)
  val tmpFile: File = File.createTempFile("test_vectors.", ".txt")
  compactText.saveKryo(tmpFile.getAbsolutePath) // default to kryo format now
  val compactBin: CompactWordEmbeddingMap = CompactWordEmbeddingMap(tmpFile.getAbsolutePath, resource = false, cached = true)
  val epsilon = 0.000001

  behavior of "compactText version"

  it should "have the same size" in {
    val compactRows = compactText.rows + compactText.unkEmbeddingOpt.map(_ => 1).getOrElse(0)

    compactRows should be (fullsizeText.matrix.size)
    compactText.columns should be (fullsizeText.dimensions)
  }

  it should "have the same contents" in {
    val compactSet = compactText.knownKeys.toSet ++ compactText.unkEmbeddingOpt.map(_ => Set(CompactWordEmbeddingMap.UNK)).getOrElse(Set.empty)

    compactSet should be (fullsizeText.matrix.keys.toSet)

    compactText.knownKeys.foreach { key =>
      val compactResult = compactText.get(key).get
      val fullsizeResult = fullsizeText.matrix(key)

      matches(compactResult, fullsizeResult, epsilon) should be (true)
    }
    if (compactText.unkEmbeddingOpt.isDefined)
      matches(compactText.unkEmbeddingOpt.get, fullsizeText.matrix(CompactWordEmbeddingMap.UNK), epsilon) should be (true)
  }

  it should "be normalized" in {
    0.until(compactText.rows).foreach { row =>
      val result = compactText.dotProduct(row, row)

      (math.abs(result - 1) < epsilon) should be (true)
    }
  }

  it should "get the same results" in {
    val sanitizer = WordEmbeddingMap.defaultWordSanitizer

    compactText.knownKeys.foreach { key1 =>
      compactText.knownKeys.foreach { key2 =>

        val result1a = compactText.avgSimilarity(
          Array(sanitizer.sanitizeWord(key1)),
          Array(sanitizer.sanitizeWord(key2))
        )
        val result2a = fullsizeText.avgSimilarity(Array(key1), Array(key2))

        (math.abs(result1a - result2a) < epsilon) should be(true)

        val result1b = compactText.avgSimilarity(
          Array(sanitizer.sanitizeWord(key1), sanitizer.sanitizeWord(key2)),
          Array(sanitizer.sanitizeWord(key2), sanitizer.sanitizeWord(key1))
        )
        val result2b = fullsizeText.avgSimilarity(Array(key1, key2), Array(key2, key1))

        (math.abs(result1b - result2b) < epsilon) should be(true)
      }
    }
  }


  behavior of "compactBin version"

  it should "have the same size" in {
    compactText.rows should be (compactBin.rows)
    compactText.columns should be (compactBin.columns)
  }

  it should "have the same contents" in {
    compactText.knownKeys should be (compactBin.knownKeys)

    compactText.knownKeys.foreach { key =>
      matches(compactText.get(key).get, compactBin.get(key).get) should be (true)
    }
  }

  it should "get the same results" in {
    compactText.knownKeys.foreach { key1 =>
      compactText.knownKeys.foreach { key2 =>

        val result1a = compactText.avgSimilarity(Array(key1), Array(key2))
        val result2a = compactBin.avgSimilarity(Array(key1), Array(key2))

        (math.abs(result1a - result2a) == 0) should be(true)

        val result1b = compactText.avgSimilarity(Array(key1, key2), Array(key2, key1))
        val result2b = compactBin.avgSimilarity(Array(key1, key2), Array(key2, key1))

        (math.abs(result1b - result2b) == 0) should be(true)
      }
    }
  }
}
