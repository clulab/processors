package org.clulab.utils

import scala.collection.compat.IterableFactoryExtensionMethods
import scala.collection.compat.immutable.{ArraySeq => ImmutableArraySeq}
import scala.collection.immutable.{Vector => ImmutableVector}
import scala.collection.mutable.{ArraySeq => MutableArraySeq}
import scala.collection.mutable.{WrappedArray => MutableWrappedArray}

class TestArray extends Test {

  def processSeq(seq: Seq[Int]): Seq[Int] = seq
  def processIndexedSeq(indexedSeq: IndexedSeq[Int]): IndexedSeq[Int] = indexedSeq

  // Notes
  // readableSeq, array.toSeq
  // readableIndexedSeq, array.toIndexedSeq but it creates a copy!
  // writeableSeq, mutable.ArraySeq()
  // writeableIndexedSeq, mutable.ArraySeq()

  val standard = "work as expected without import"
  val custom = "work as expected with import"
  val array = Array(1, 2, 3, 4, 5)

  behavior of "Array.toSeq"
  it should standard in {
    val seq = array.toSeq
    seq shouldNot be theSameInstanceAs array
    seq shouldBe a [MutableWrappedArray[_]]
    seq.asInstanceOf[MutableWrappedArray[Int]].array should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val seq = array.toSeq
    seq shouldNot be theSameInstanceAs array
    seq shouldBe a [MutableWrappedArray[_]]
    seq.asInstanceOf[MutableWrappedArray[Int]].array should be theSameInstanceAs array
  }

  behavior of "Array.toIndexedSeq"
  it should standard in {
    val indexedSeq = array.toIndexedSeq
    indexedSeq shouldNot be theSameInstanceAs array
    indexedSeq shouldBe an [ImmutableVector[_]]
    indexedSeq.asInstanceOf[ImmutableVector[_]].toArray shouldNot be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val indexedSeq = array.toIndexedSeq
    indexedSeq shouldNot be theSameInstanceAs array
    indexedSeq shouldBe an [ImmutableVector[_]]
    indexedSeq.asInstanceOf[ImmutableVector[_]].toArray shouldNot be theSameInstanceAs array
  }

  behavior of "MutableArraySeq.from"
  it should standard in {
    val mutableArraySeqArray = MutableArraySeq.from(array).array
    mutableArraySeqArray shouldNot be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val mutableArraySeqArray = MutableArraySeq.from(array).array
    mutableArraySeqArray shouldNot be theSameInstanceAs array
  }

  behavior of "ImmutableArraySeq.unsafeWrapArray"
  it should standard in {
    val immutableArraySeqArray = ImmutableArraySeq.unsafeWrapArray(array).unsafeArray
    immutableArraySeqArray should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val immutableArraySeqArray = ImmutableArraySeq.unsafeWrapArray(array).unsafeArray
    immutableArraySeqArray should be theSameInstanceAs array
  }

  behavior of "processSeq(array)"
  it should standard in {
    val autoCopySeq = processSeq(array)
    autoCopySeq shouldNot be theSameInstanceAs array
    autoCopySeq shouldBe a [MutableWrappedArray[_]]
    autoCopySeq.asInstanceOf[MutableWrappedArray[Int]].array should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val autoCopySeq = processSeq(array)
    autoCopySeq shouldNot be theSameInstanceAs array
    autoCopySeq shouldBe a [MutableWrappedArray[_]]
    autoCopySeq.asInstanceOf[MutableWrappedArray[Int]].array should be theSameInstanceAs array
  }

  behavior of "processSeq(ImmutableArraySeq.unsafeWrapArray)"
  it should standard in {
    val autoCopySeq = processSeq(ImmutableArraySeq.unsafeWrapArray(array))
    autoCopySeq shouldNot be theSameInstanceAs array
    autoCopySeq shouldBe an [ImmutableArraySeq[_]]
    autoCopySeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val autoCopySeq = processSeq(ImmutableArraySeq.unsafeWrapArray(array))
    autoCopySeq shouldNot be theSameInstanceAs array
    autoCopySeq shouldBe an [ImmutableArraySeq[_]]
    autoCopySeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }

  behavior of "processIndexedSeq(array)"
  it should standard in {
    val autoCopyIndexedSeq = processIndexedSeq(array)
    autoCopyIndexedSeq shouldNot be theSameInstanceAs array
    autoCopyIndexedSeq shouldBe a [MutableWrappedArray[_]]
    autoCopyIndexedSeq.asInstanceOf[MutableWrappedArray[Int]].array should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val autoCopyIndexedSeq = processIndexedSeq(array)
    autoCopyIndexedSeq shouldNot be theSameInstanceAs array
    autoCopyIndexedSeq shouldBe a [MutableWrappedArray[_]]
    autoCopyIndexedSeq.asInstanceOf[MutableWrappedArray[Int]].array should be theSameInstanceAs array
  }

  behavior of "processIndexedSeq(ImmutableArraySeq.unsafeWrapArray)"
  it should standard in {
    val autoCopyIndexedSeq = processIndexedSeq(ImmutableArraySeq.unsafeWrapArray(array))
    autoCopyIndexedSeq shouldNot be theSameInstanceAs array
    autoCopyIndexedSeq shouldBe an [ImmutableArraySeq[_]]
    autoCopyIndexedSeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val autoCopyIndexedSeq = processIndexedSeq(ImmutableArraySeq.unsafeWrapArray(array))
    autoCopyIndexedSeq shouldNot be theSameInstanceAs array
    autoCopyIndexedSeq shouldBe an [ImmutableArraySeq[_]]
    autoCopyIndexedSeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }

  behavior of ":Seq[]"
  it should standard in {
    val seq: Seq[Int] = array
    seq.toArray should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val seq: Seq[Int] = array
    seq.toArray should be theSameInstanceAs array
  }

  behavior of ":IndexedSeq[]"
  it should standard in {
    val indexedSeq: IndexedSeq[Int] = array
    indexedSeq.toArray should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val indexedSeq: IndexedSeq[Int] = array
    indexedSeq.toArray should be theSameInstanceAs array
  }

  behavior of "foreach"
  it should standard in {
    def foreach[T](array: Array[T]): Unit = {
      array.foreach { each => () }
    }

    foreach(array)
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    def foreach[T](array: Array[T]): Unit = {
      array.foreach { each => () }
    }

    foreach(array)
  }

  behavior of "zip"
  it should standard in {
    def zip[T](array: Array[T]): Unit = {
      array.zip(array)
    }

    zip(array)
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    def zip[T](array: Array[T]): Unit = {
      array.zip(array)
    }

    zip(array)
  }
}
