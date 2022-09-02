package org.clulab.utils

import scala.collection.{immutable, mutable}
import org.clulab.scala.WrappedArray

class TestArray extends Test {

  behavior of "Arrays"

  def processSeq(seq: Seq[Int]): Seq[Int] = seq
  def processIndexedSeq(indexedSeq: IndexedSeq[Int]): IndexedSeq[Int] = indexedSeq

  // Notes
  // readableSeq, immutable.ArraySeq
  // readableIndexedSeq, immutable.ArraySeq
  // writeableSeq, mutable.ArraySeq()
  // writeableIndexedSeq, mutable.ArraySeq()

  // new ArrayOps(xs).toIndexedSeq
  // immutable.ArraySeq.unsafeWrapArray(Array.copyOf(xs, xs.length))
  // This means that a copy is produced.  We should instead way
  // ArraySeq.unsafeWrapArray(array) so that no copy is produced.

  it should "work as expected with default conversions" in {
    val array = Array(1, 2, 3, 4, 5)
    val seq = array.toSeq // calls new ArrayOps(xs).toIndexedSeq
    val indexedSeq = array.toIndexedSeq // calls new ArrayOps(xs).toIndexedSeq
    val mutableArraySeq = mutable.ArraySeq.from(array)
    val immutableArraySeq = immutable.ArraySeq.unsafeWrapArray(array) // quick
    val autoCopySeq1 = processSeq(array) // calls new ArrayOps(xs).toIndexedSeq
    val autoCopySeq2 = processSeq(immutable.ArraySeq.unsafeWrapArray(array))
    val autoCopyIndexedSeq1 = processIndexedSeq(array)
    val autoCopyIndexedSeq2 = processIndexedSeq(immutable.ArraySeq.unsafeWrapArray(array))

    seq should (contain theSameElementsInOrderAs array)
    indexedSeq should (contain theSameElementsInOrderAs array)
    mutableArraySeq should (contain theSameElementsInOrderAs array)
    immutableArraySeq should (contain theSameElementsInOrderAs array)
    autoCopySeq1 should (contain theSameElementsInOrderAs array)
    autoCopySeq2 should (contain theSameElementsInOrderAs array)
    autoCopyIndexedSeq1 should (contain theSameElementsInOrderAs array)
    autoCopyIndexedSeq1 should (contain theSameElementsInOrderAs array)

    array(1) = 7

    seq should not(contain theSameElementsInOrderAs array)
    indexedSeq should not(contain theSameElementsInOrderAs array)
    mutableArraySeq should not(contain theSameElementsInOrderAs array)
    immutableArraySeq should (contain theSameElementsInOrderAs array) // should!
    autoCopySeq1 should not (contain theSameElementsInOrderAs array)
    autoCopySeq2 should (contain theSameElementsInOrderAs array) // should!
    autoCopyIndexedSeq1 should not (contain theSameElementsInOrderAs array)
    autoCopyIndexedSeq2 should (contain theSameElementsInOrderAs array) // should!

    val wrapped2a: Seq[Int] = array // processSeq(wrapped2)
    val wrapped2b: IndexedSeq[Int] = array // processIndexedSeq(wrapped2)

    wrapped2a.toArray.eq(array) should not be (true)
    wrapped2b.toArray.eq(array) should not be (true)

    val wrapped3 = immutable.ArraySeq.unsafeWrapArray(array)
    val wrapped3a = wrapped3.unsafeArray

    wrapped3a.eq(array) should be (true)
  }

  it should "work as expected with custom conversions" in {
    import org.clulab.scala.WrappedArray._ // add custom conversions

    val array = Array(1, 2, 3, 4, 5)
    val seq = array.toSeq // calls new ArrayOps(xs).toIndexedSeq
    val indexedSeq = array.toIndexedSeq // calls new ArrayOps(xs).toIndexedSeq
    val mutableArraySeq = mutable.ArraySeq.from(array)
    val immutableArraySeq = immutable.ArraySeq.unsafeWrapArray(array) // quick
    val autoCopySeq1 = processSeq(array) // calls new ArrayOps(xs).toIndexedSeq
    val autoCopySeq2 = processSeq(immutable.ArraySeq.unsafeWrapArray(array))
    val autoCopyIndexedSeq1 = processIndexedSeq(array)
    val autoCopyIndexedSeq2 = processIndexedSeq(immutable.ArraySeq.unsafeWrapArray(array))

    seq should (contain theSameElementsInOrderAs array)
    indexedSeq should (contain theSameElementsInOrderAs array)
    mutableArraySeq should (contain theSameElementsInOrderAs array)
    immutableArraySeq should (contain theSameElementsInOrderAs array)
    autoCopySeq1 should (contain theSameElementsInOrderAs array)
    autoCopySeq2 should (contain theSameElementsInOrderAs array)
    autoCopyIndexedSeq1 should (contain theSameElementsInOrderAs array)
    autoCopyIndexedSeq1 should (contain theSameElementsInOrderAs array)

    array(1) = 7

    seq should not(contain theSameElementsInOrderAs array)
    indexedSeq should not(contain theSameElementsInOrderAs array)
    mutableArraySeq should not(contain theSameElementsInOrderAs array)
    immutableArraySeq should (contain theSameElementsInOrderAs array) // should!
    autoCopySeq1 should (contain theSameElementsInOrderAs array) // should!
    autoCopySeq2 should (contain theSameElementsInOrderAs array) // should!
    autoCopyIndexedSeq1 should (contain theSameElementsInOrderAs array) // should!
    autoCopyIndexedSeq2 should (contain theSameElementsInOrderAs array) // should!

    val wrapped2a: Seq[Int] = array // processSeq(wrapped2)
    val wrapped2b: IndexedSeq[Int] = array // processIndexedSeq(wrapped2)

    wrapped2a.toArray.eq(array) should not be (true)
    wrapped2b.toArray.eq(array) should not be (true)

    val wrapped3 = immutable.ArraySeq.unsafeWrapArray(array)
    val wrapped3a = wrapped3.unsafeArray

    wrapped3a.eq(array) should be(true)
  }
}
