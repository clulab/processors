package org.clulab.utils

import scala.collection.compat.IterableFactoryExtensionMethods
import scala.collection.compat.immutable
import scala.collection.mutable

class TestArray extends Test {

  behavior of "Arrays"

  def processSeq(seq: Seq[Int]): Seq[Int] = seq

  def processIndexedSeq(indexedSeq: IndexedSeq[Int]): IndexedSeq[Int] = indexedSeq

  // This should achieve indexable without a copy.
  def toWrapped(array: Array[Int]): IndexedSeq[Int] = {

    def simulate(indexedSeq: IndexedSeq[Int]): IndexedSeq[Int] = indexedSeq

    val simulated = simulate(array)

    assert(simulated.getClass.getName == "scala.collection.mutable.WrappedArray$ofInt")
    assert(simulated.toArray.eq(array))
    simulated
  }

  // readableSeq, array.toSeq
  // readableIndexedSeq, array.toIndexedSeq but it creates a copy!
  // writeableSeq, mutable.ArraySeq()
  // writeableIndexedSeq, mutable.ArraySeq()

  // The immutable.ArraySeq is not available in Scala 2.12
  it should "work as expected" in {
    val array = Array(1, 2, 3, 4, 5)

    val seq = array.toSeq // dangerous
    val indexedSeq = array.toIndexedSeq // This makes a copy!
    val mutableArraySeq = mutable.ArraySeq.from(array) // This makes a copy!
    val immutableArraySeq = immutable.ArraySeq.unsafeWrapArray(array)
    val autoCopySeq1 = processSeq(array) // dangerous
    val autoCopySeq2 = processSeq(immutable.ArraySeq.unsafeWrapArray(array)) // dangerous
    val autoCopyIndexedSeq1 = processIndexedSeq(array) // dangerous
    val autoCopyIndexedSeq2 = processIndexedSeq(immutable.ArraySeq.unsafeWrapArray(array)) // dangerous

    val wrapped = toWrapped(array)
    val originalArray: Array[Int] = wrapped.toArray // This must be explicit.

    seq should (contain theSameElementsInOrderAs array) // WrappedArray
    indexedSeq should (contain theSameElementsInOrderAs array) // Vector!
    mutableArraySeq should (contain theSameElementsInOrderAs array) // ArraySeq
    immutableArraySeq should (contain theSameElementsInOrderAs array) // ArraySeq
    autoCopySeq1 should (contain theSameElementsInOrderAs array) // WrappedArray
    autoCopySeq2 should (contain theSameElementsInOrderAs array) // WrappedArray
    autoCopyIndexedSeq1 should (contain theSameElementsInOrderAs array) // Vector
    autoCopyIndexedSeq2 should (contain theSameElementsInOrderAs array) // Vector
    wrapped should (contain theSameElementsInOrderAs array) // WrappedArray

    array(1) = 7

    // The WrappedArrays will reflect the change.
    seq should (contain theSameElementsInOrderAs array)
    indexedSeq should not (contain theSameElementsInOrderAs array)
    mutableArraySeq should not (contain theSameElementsInOrderAs array)
    immutableArraySeq should (contain theSameElementsInOrderAs array)
    // This next one differs from Scala 2.13!
    autoCopySeq1 should (contain theSameElementsInOrderAs array)
    autoCopySeq2 should (contain theSameElementsInOrderAs array)
    // This next one differs from Scala 2.13!
    autoCopyIndexedSeq1 should (contain theSameElementsInOrderAs array)
    autoCopyIndexedSeq2 should (contain theSameElementsInOrderAs array)
    wrapped should (contain theSameElementsInOrderAs array)

    import org.clulab.scala.Wrapped
    val wrapped2 = array.toWrapped
    processSeq(wrapped2)
    processIndexedSeq(wrapped2)
  }
}
