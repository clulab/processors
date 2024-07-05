package org.clulab.struct

trait HeapElement {
  def score: Float
}

class MinHeap(val maxSize:Int = -1) {
  private var heap: Array[HeapElement] = Array()
  private var size: Int = 0

  def getMin: Option[HeapElement] = {
    if (size == 0) None else Some(heap(0))
  }

  def elementSequence: Seq[HeapElement] = heap.slice(0, size)

  def isEmpty: Boolean = size == 0
  def nonEmpty: Boolean = size > 0

  def insert(value: HeapElement): Boolean = {
    // special cases if size == maxSize
    if(maxSize > 0 && size == maxSize) {
      // do not insert if the new element is smaller than getMin
      if(value.score < getMin.get.score) {
        return false // we return false when nothing was inserted
      } 
      // remove the smallest and then insert
      else {
        heap(0) = value
        heapifyDown(0)
        return true
      }
    } else {
      // normal insert; we have room to grow
      if (size == heap.length) {
        heap = if (heap.isEmpty) new Array[HeapElement](1) else heap ++ Array.ofDim[HeapElement](heap.length)
      }
      heap(size) = value
      size += 1
      heapifyUp(size - 1)
      return true
    }
  }

  def extractMin(): Option[HeapElement] = {
    if (size == 0) return None
    val root = heap(0)
    heap(0) = heap(size - 1)
    size -= 1
    heapifyDown(0)
    Some(root)
  }

  private def heapifyUp(index: Int): Unit = {
    var i = index
    while (i > 0 && heap(parent(i)).score > heap(i).score) {
      swap(i, parent(i))
      i = parent(i)
    }
  }

  private def heapifyDown(index: Int): Unit = {
    var smallest = index
    val leftChild = left(index)
    val rightChild = right(index)

    if (leftChild < size && heap(leftChild).score < heap(smallest).score) {
      smallest = leftChild
    }

    if (rightChild < size && heap(rightChild).score < heap(smallest).score) {
      smallest = rightChild
    }

    if (smallest != index) {
      swap(index, smallest)
      heapifyDown(smallest)
    }
  }

  private def parent(index: Int): Int = (index - 1) / 2
  private def left(index: Int): Int = 2 * index + 1
  private def right(index: Int): Int = 2 * index + 2

  private def swap(i: Int, j: Int): Unit = {
    val temp = heap(i)
    heap(i) = heap(j)
    heap(j) = temp
  }

  /** Returns the heap as a sequence of elements, sorted in descending order of scores */
  def toSortedSeq: Seq[HeapElement] = {
    heap.slice(0, size).sortBy(- _.score)
  }
}

class MinHeapIterator[HeapElement](heap: MinHeap) extends Iterator[HeapElement] {
  var position = 0
  val elements = heap.elementSequence

  def hasNext: Boolean = position < elements.size
    
  def next(): HeapElement = {
    if(position >= elements.size) {
      throw new RuntimeException("ERROR: MinHeapIterator.next() outside of bounds!")
    }

    val crt = elements(position).asInstanceOf[HeapElement]
    position += 1

    crt
  }
}

