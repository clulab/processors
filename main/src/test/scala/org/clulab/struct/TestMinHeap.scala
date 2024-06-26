package org.clulab.struct

import org.clulab.utils.Test

case class FHE(score: Float) extends HeapElement

class TestMinHeap extends Test {
  "MinHeap" should "order elements properly" in {
    val minHeap = new MinHeap()

    minHeap.insert(FHE(3))
    minHeap.insert(FHE(1))
    minHeap.insert(FHE(6))
    minHeap.insert(FHE(5))
    minHeap.insert(FHE(2))
    minHeap.insert(FHE(4))

    minHeap.extractMin().get.score should be (1) // Some(1)
    minHeap.extractMin().get.score should be (2) // Some(2)
    minHeap.extractMin().get.score should be (3) // Some(3)
    minHeap.extractMin().get.score should be (4) // Some(4)
    minHeap.extractMin().get.score should be (5) // Some(5)
    minHeap.extractMin().get.score should be (6) // Some(6)
    minHeap.extractMin().isEmpty should be(true) // None
  }

  it should "keep correct top K" in {
    val minHeap = new MinHeap(3)

    minHeap.insert(FHE(3))
    minHeap.insert(FHE(1))
    minHeap.insert(FHE(6))
    minHeap.insert(FHE(5))
    minHeap.insert(FHE(2))
    minHeap.insert(FHE(4))

    minHeap.extractMin().get.score should be (4) 
    minHeap.extractMin().get.score should be (5) 
    minHeap.extractMin().get.score should be (6) 
    minHeap.extractMin().isEmpty should be (true) 
  }
}
